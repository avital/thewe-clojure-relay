(ns we
  (:use clojure.contrib.json.read)
  (:use clojure.contrib.json.write)
  (:use clojure.set)
  (:use clojure.contrib.duck-streams)
  (:import java.util.Date))

; =====================
; ======= Atoms =======
; =====================

(defmacro init-atoms 
  ([] nil)
  ([name initial-val & rest]
     `(do
        (defonce ~name (atom ~initial-val))
        ~`(init-atoms ~@rest))))

(init-atoms *rep-rules* #{}
            *clipboard* nil
            *last-clipboard* nil
            *other-wave* nil)

; =========================
; ======= Utilities =======
; =========================

; (dig {:a {:b 3}} :a :b) returns 3
(defn dig [map & rest]
  (get-in map rest))

(def log-list (atom []))

; From clojure.contrib.core. Not sure why I can't just use it.
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
nested structure. keys is a sequence of keys. Any empty maps that result
will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
	    (assoc m k newmap)
	    (dissoc m k)))
      m)
    (dissoc m k)))


; =========================================
; ======= Logical Replication Layer =======
; =========================================

; Data structures:
; ----------------
;
; rep-loc:    Either {:type "blip" :wave-id wave-id :wavelet-id wavelet-id :blip-id blip-id} or
;                    {:type "gadget" :wave-id wave-id :wavelet-id wavelet-id :blip-id blip-id :key key} or
;
; rep-op:     {:rep-loc rep-loc :content content :action action :loc-type: gadget} or
;             {:rep-loc rep-loc :content content :action action :loc-type: blip}
;
; rep-rules:  A set of sets (a partition) of rep-locs

(defn-log add-to-class [partition class el] 
  (conj (disj partition class) (conj class el)))

(defn-log containing-rep-class [rep-loc]
  (first (for [rep-class @*rep-rules* :when (some #{rep-loc} rep-class)] rep-class)))

(defn-log replicate-replocs! [r1 r2]
  (let [rc1 (containing-rep-class r1) rc2 (containing-rep-class r2)]
    (cond
      (and (not rc1) (not rc2))  ; when both are not in rep-classes
      (swap! *rep-rules* conj #{r1 r2})

      (and rc1 (not rc2))
      (swap! *rep-rules* add-to-class rc1 r2)

      (and rc2 (not rc1))
      (swap! *rep-rules* add-to-class rc2 r1)

      (and (and rc1 rc2) (not= rc1 rc2))
      (swap! *rep-rules* #(conj (disj % rc1 rc2) (union rc1 rc2))))))

(defn-log equal-rep-loc [r1 r2]
  (let [rep-loc-keys [:wave-id :wavelet-id :blip-id]] 
    (= (select-keys r1 rep-loc-keys) (select-keys r2 rep-loc-keys))))

(defmulti update-rep-loc-ops
  (fn-log [rep-loc content] (:type rep-loc)))

; @todo: can this be better?
; checks whether the rep-op satisfies the rep-loc definition
(defn-log match-rep-loc [rep-op rep-loc]
  (if (:blip-id rep-loc)
    (= (:rep-loc rep-op) rep-loc)
    (and
     (= (dissoc (:rep-loc rep-op) :blip-id) (dissoc rep-loc :blip-id :subcontent))
     (.contains (:content rep-op) (:subcontent rep-loc)))))

; Receives rep-rules and incoming rep-ops and returns rep-ops to be acted upon
(defn-log do-replication [rep-rules rep-ops]
  (apply concat (for [rep-op rep-ops
		      rep-class rep-rules :when (some (partial match-rep-loc rep-op) rep-class)
		      rep-loc rep-class :when (and (rep-loc :blip-id) (not= rep-loc (:rep-loc rep-op)))]
		  (update-rep-loc-ops rep-loc (:content rep-op)))))


; ===============================================
; ======= Google Wave Incoming JSON Layer =======
; ===============================================

; Data structures:
; ----------------
;
; incoming-map:       Some crazy Google format of a map that contains information on which
;                     blips were modified, their content (including gadgets inside)
;                     and their parent blip contents
;
; blip-data:          (dig incoming-wave-map "blips" "map" blip-id)

(defn-log blip-data-to-rep-ops [blip-data]
  (let [basic-rep-loc {:wave-id (blip-data "waveId"), :wavelet-id (blip-data "waveletId"), :blip-id (blip-data "blipId")}]
    (if-let [gadget-map (first (dig blip-data "elements" "map"))]
					; there is a gadget here
      (let [gadget-state (dig (val gadget-map) "properties" "map")]
        (for [[k v] gadget-state]
          {:rep-loc (assoc basic-rep-loc :type "gadget" :key k) :content v}))

					; there is no gadget
      [{:rep-loc (assoc basic-rep-loc :type "blip") :content (blip-data "content")}])))

(defn-log incoming-map-to-rep-ops [incoming]
  (let [modified-blip-ids
        (for [event (dig incoming "events" "list")
              :when (not (.endsWith (event "modifiedBy") "@a.gwave.com"))
              :when (= (event "type") "BLIP_SUBMITTED")]
          (dig event "properties" "map" "blipId"))]
    (apply concat
      (for [blip-id modified-blip-ids]
        (blip-data-to-rep-ops
          (dig incoming "blips" "map" blip-id))))))


; ===============================================
; ======= Google Wave Outgoing JSON Layer =======
; ===============================================

; Data structures:
; ----------------
;
; outgoing-map:  Some crazy Google format of a map that contains information on which
;                operations the robot will do

(defn-log op-skeleton [rep-loc] 
  (let [wave-id (:wave-id rep-loc)
	wavelet-id (:wavelet-id rep-loc)
	blip-id (:blip-id rep-loc)]
    {"waveId" wave-id
     "waveletId" wavelet-id
     "blipId" blip-id
     "javaClass"  "com.google.wave.api.impl.OperationImpl"}))

(defn-log document-delete-append-ops [rep-loc content]
  [(assoc (op-skeleton rep-loc)
     "index"  -1,
      "property"  nil,
      "type"  "DOCUMENT_DELETE")
   (assoc (op-skeleton rep-loc)
     "index"  0,
      "property"  content,
      "type"  "DOCUMENT_APPEND")])

(defn-log document-delete-ops [rep-loc]
  [(assoc (op-skeleton rep-loc)
     "index"  -1,
     "property"  nil,
     "type"  "DOCUMENT_DELETE")])

(defn-log range-op-json [start end] 
  {"end" end, "javaClass" "com.google.wave.api.Range", "start" start})

(defn-log annotation-op-json [name start end value]
  {"javaClass" "com.google.wave.api.Annotation",
   "value" value,
   "name" name,
   "range" (range-op-json start end)})

(defn-log delete-annotation-ops [rep-loc start end]
  [(assoc (op-skeleton rep-loc)
     "index" -1,
     "property" (range-op-json start end),
     "type" "DOCUMENT_ANNOTATION_DELETE")])

(defn-log add-annotation-ops [rep-loc name start end value]
  [(assoc (op-skeleton rep-loc)
     "index" 0,
     "property" (annotation-op-json name start end value),
     "type" "DOCUMENT_ANNOTATION_SET")])

; @todo: what is the difference between using "append" and :append?

(defn-log document-insert-ops [rep-loc cursor content]
  [(assoc (op-skeleton rep-loc) 
     "index"  cursor,
     "property"  content,
     "type"  "DOCUMENT_INSERT")])

(defn-log gadget-op-json [gadget-state]
  {"javaClass" "com.google.wave.api.Gadget",
   "properties" 
   {"map" gadget-state
    "javaClass" "java.util.HashMap"},
   "type" "GADGET"})

(defn-log append-gadget-ops [rep-loc gadget-state]
  [(assoc (op-skeleton rep-loc)
     "index" 0,
     "property" (gadget-op-json gadget-state),
     "type" "DOCUMENT_ELEMENT_APPEND")])

(defn-log gadget-submit-delta-1-op [rep-loc state]
  (assoc (op-skeleton rep-loc)
    "index" -1,
    "property" (gadget-op-json state),
    "type" "DOCUMENT_ELEMENT_MODIFY_ATTRS"))

(defn-log gadget-submit-delta-ops [rep-loc state]
  [(gadget-submit-delta-1-op rep-loc 
			     (into {} 
				   (for [[key val] state] 
				     [key (if (= key "url") val "x")]))) 
   (gadget-submit-delta-1-op rep-loc state)])

(defn-log blip-data-op-json [rep-loc content]
  (assoc (op-skeleton rep-loc)
    "lastModifiedTime" -1,
    "contributors" {"javaClass" "java.util.ArrayList",
		    "list" []},
    "parentBlipId" nil,
    "version" -1,
    "creator" nil,
    "content" content,
    "javaClass" "com.google.wave.api.impl.BlipData",
    "annotations" {"javaClass" "java.util.ArrayList",
		   "list" []},
    "elements" {"map" {},"javaClass" "java.util.HashMap"},
    "childBlipIds" {"javaClass" "java.util.ArrayList",
		    "list" []}))

(defn-log blip-create-child-ops [rep-loc content new-id]
  [(assoc (op-skeleton rep-loc) 
     "index" -1,
     "property" (blip-data-op-json (assoc rep-loc :blip-id new-id) content),
     "type" "BLIP_CREATE_CHILD")])

(defn operation-bundle-json [ops]
  {"javaClass"  "com.google.wave.api.impl.OperationMessageBundle",
   "operations"  {"javaClass"  "java.util.ArrayList",
                  "list"  ops}
   "version"  "106"}) ; @todo deal with version


(defmethod update-rep-loc-ops "gadget" [rep-loc content]
  (log (gadget-submit-delta-ops rep-loc 
				{(:key rep-loc) content
				 "url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml"})))

(defmethod update-rep-loc-ops "blip" [rep-loc content]
  (log (document-delete-append-ops rep-loc content)))

(defn-log add-string-and-eval-ops [rep-loc str]
  (concat
   (document-insert-ops rep-loc 0 str)
   (add-annotation-ops rep-loc "we/eval" 0 (count str) "nothing")))


; =============================
; ======= Harness Layer =======
; =============================

;;; Utilities



;;; Helper "API"

(def *ctx*)

(defmacro iterate-events [events listen-to for-args]
  `(let [~'modified-blip-ids
	 (for [~'event (dig ~events "events" "list")
	       :when (not (.endsWith (~'event "modifiedBy") "@a.gwave.com"))
	       :when (= (~'event "type") ~listen-to)]
	   (dig ~'event "properties" "map" "blipId"))]
     (for [~'blip-id ~'modified-blip-ids
	   :let [~'blip-data (dig ~events "blips" "map" ~'blip-id)
		 ~'content (~'blip-data "content")		 
		 ~'blip-annotations (dig ~'blip-data "annotations" "list")		 
		 ~'rep-loc {:type "blip"  :wave-id (~'blip-data "waveId") :wavelet-id (~'blip-data "waveletId") :blip-id (~'blip-data "blipId")}
		 ~'first-gadget-map (first (dig ~'blip-data "elements" "map"))
		 ~'gadget-state (if ~'first-gadget-map (dig (val ~'first-gadget-map) "properties" "map") {})]] 
       (binding [~'*ctx* {:rep-loc ~'rep-loc :content ~'content :annotations ~'blip-annotations :gadget-state ~'gadget-state}] ~for-args))))



;;; Functions that can be called with an we/eval

(defn-log delete-annotation [annotation]
  (delete-annotation-ops (:rep-loc *ctx*) 
	 (dig annotation "range" "start")
	 (dig annotation "range" "end")))

(defn-log echo [s]
  (document-insert-ops (:rep-loc *ctx*) (:cursor *ctx*) (str \newline s)))

(defn-log echo-pp [s]
  (echo (pprn-str s)))

(defn-log identify-this-blip []
  (echo-pp (:rep-loc *ctx*)))

(defn-log create-child-blip [] 
  (blip-create-child-ops (:rep-loc *ctx*) "" (str (rand))))

(defn-log modify-ggg [key val]
  (gadget-submit-delta-ops (:rep-loc *ctx*) {key val "url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml"}))

(defn-log current-rep-class []
  (containing-rep-class (*ctx* :rep-loc)))

(defn-log gadget-rep-class [key]
  (containing-rep-class (assoc (*ctx* :rep-loc)
			   :type "gadget" :key key)))


;;; Clipboard stuff

(defn-log remember-wave! []
  (reset! *other-wave* *ctx*)
  (echo "Remembered"))

(defmacro on-other-wave [expr]
  `(binding [*ctx* @*other-wave*]
     ~expr))

(defn-log remember-gadget-key! [rep-key]
  (reset! *clipboard* 
	  {:source-key rep-key 
	   :rep-loc (:rep-loc *ctx*) 
	   :subkeys (for [[key val] (:gadget-state *ctx*) 
			  :when (or (= key rep-key) (.startsWith key (str rep-key ".")))]
		      [(.replaceFirst key rep-key "") val])})
 ; this never happened
  (reset! *last-clipboard* @*clipboard*)
  (echo "ok!"))

(defn submit-replication-delta [rep-key]
  (we/gadget-submit-delta-ops (:rep-loc we/*ctx*) (into {"url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml"} (for [[subkey val] (:subkeys @we/*last-clipboard*)] [(str rep-key subkey) val]))))

(defn-log replicate-gadget-key! [rep-key]
  (doseq [:let [{subkeys :subkeys source-key :source-key source-rep-loc :rep-loc} @*clipboard*] [subkey _] subkeys] 
    (replicate-replocs!     
     (assoc source-rep-loc :type "gadget" :key (str source-key subkey))
     (assoc (:rep-loc *ctx*) :type "gadget" :key (str rep-key subkey))))
  (add-string-and-eval-ops (:rep-loc *ctx*) (str `(we/submit-replication-delta ~rep-key))))

(defn-log create-view-dev-replication-generic! [suffix]
  (let [rep-loc (:rep-loc *ctx*)]
    (replicate-replocs!
     (assoc rep-loc :type "gadget" :key "_view.js")
     (dissoc (assoc rep-loc :subcontent (str  "// " suffix "js")) :blip-id))
    
    (replicate-replocs!
     (assoc rep-loc :type "gadget" :key "_view.html")
     (dissoc (assoc rep-loc :subcontent (str "<!-- html" suffix " -->")) :blip-id))
    
    (replicate-replocs!
     (assoc rep-loc :type "gadget" :key "_view.css")
     (dissoc (assoc rep-loc :subcontent (str "/* css" suffix " */")) :blip-id)))
  [])

(defn-log create-view-dev-replication [] (create-view-dev-replication-generic! ""))

; @TODO this has swap! here -  is there a way to prevent it?
(defn-log view-dev-this-blip-generic
  "suffix is what we add to the content of the created blips in order to later identify them for replication by subcontent" 
  [suffix]
  (create-view-dev-replication-generic! suffix)
  (let [rep-loc (:rep-loc *ctx*)]
    (concat 
     (append-gadget-ops rep-loc 
                        {"url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml",
                         "author" "avital@wavesandbox.com"
                         "_view.js" "// js"
                         "_view.html" "<!-- html -->"
                         "_view.css" "/* css */"
                         "_rep-loc.waveId" (rep-loc :wave-id)
                         "_rep-loc.waveletId" (rep-loc :wavelet-id)
                         "_rep-loc.blipId" (rep-loc :blip-id)})
     (blip-create-child-ops rep-loc "" "html")
     (blip-create-child-ops (assoc rep-loc :blip-id "html") "" "css")
     (blip-create-child-ops (assoc rep-loc :blip-id "css") "" "js")
     (document-delete-append-ops (assoc rep-loc :blip-id "html") (str "<!-- html" suffix " -->"))
     (document-delete-append-ops (assoc rep-loc :blip-id "css") (str "/* css" suffix " */"))
     (document-delete-append-ops (assoc rep-loc :blip-id "js") (str "// " suffix "js")))))

; @TODO this has swap! here -  is there a way to prevent it?
(defn-log view-dev-this-blip-generic2
  "suffix is what we add to the content of the created blips in order to later identify them for replication by subcontent" 
  [suffix]
  (create-view-dev-replication-generic! suffix)
  (let [rep-loc (:rep-loc *ctx*)]
    (concat 
     (append-gadget-ops rep-loc 
		       {"url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml",
			"author" "avital@wavesandbox.com"
			"_view.js" ""
			"_view.html" ""
			"_view.css" ""
			;"f1._view.html" ""
			;"f1._view.css" ""
			;"f1._view.js" ""
			"_rep-loc.waveId" (rep-loc :wave-id)
			"_rep-loc.waveletId" (rep-loc :wavelet-id)
			"_rep-loc.blipId" (rep-loc :blip-id)})
     (blip-create-child-ops rep-loc "" "html")
     (blip-create-child-ops (assoc rep-loc :blip-id "html") "" "css")
     (blip-create-child-ops (assoc rep-loc :blip-id "css") "" "js")
     (document-delete-append-ops (assoc rep-loc :blip-id "html") (str "<!-- html" suffix " -->"))
     (document-delete-append-ops (assoc rep-loc :blip-id "css") (str "/* css" suffix " */"))
     (document-delete-append-ops (assoc rep-loc :blip-id "js") (str "// " suffix "js")))))


(defn-log view-dev-this-blip [] (view-dev-this-blip-generic ""))

(defn-log view-dev-annotate-blip []
  (let [rep-loc (:rep-loc *ctx*) 
	str-to-annotate "(we/view-dev-this-blip-generic2 \"2\")"]
    (concat 
     (blip-create-child-ops rep-loc "" "view-dev")
     (add-string-and-eval-ops (assoc rep-loc :blip-id "view-dev") str-to-annotate))))

(def op-map-path ["property" "properties" "map"])


;;; Utilities for gadget-initiated replication
(defn-log handle-to-key []
  (if-let [to-key ((:gadget-state *ctx*) "to-key")]
   (when (not= to-key "*")
      (reset! *clipboard* {:rep-loc (:rep-loc *ctx*) :to-key to-key})
      (gadget-submit-delta-ops (:rep-loc *ctx*) {"to-key" "*" "url" ((:gadget-state *ctx*) "url")}))))

(defn-log handle-from-key []
  (if-let [from-key ((:gadget-state *ctx*) "from-key")]  
    (if (not= from-key "*")
      (when-let [{to-key :to-key source-rep-loc :rep-loc} @*clipboard*]
	(doseq [[key val] (:gadget-state *ctx*) 
		:when (or (= key from-key) (.startsWith key (str from-key ".")))]
	  (replicate-replocs! (assoc source-rep-loc :type "gadget" :key (.replaceFirst key from-key to-key))
			      (assoc (:rep-loc *ctx*) :type "gadget" :key key)))
	(gadget-submit-delta-ops (:rep-loc *ctx*) {"from-key" "*" "url" ((:gadget-state *ctx*) "url")})))))

(defn-log handle-gadget-rep "TODO" []
  (concat
   (handle-to-key)
   (handle-from-key)))


;;; "Subrobots" to be used with thewe-0+...@appspot.com

(defn sfirst [x]
  (if-let [result (first x)]
    result
    []))

(defn-log run-function-do-operations [events-map] ; this is the signature of a function that can be called by adding + to a robot's address
  (sfirst ; this is the solution for now as there is probably no more than one evaluated expression in each event sent to us
   (iterate-events events-map "DOCUMENT_CHANGED"     
		   (apply concat 
			  (for [annotation (log (:annotations *ctx*)) 		  
				:when (not= -1 (dig annotation "range" "start"))
				:when (= "we/eval" (annotation "name"))
				:let [start (dig annotation "range" "start") 
				      end (dig annotation "range" "end")]] 
			    (binding [*ctx* (assoc *ctx* :cursor end)]
			      (concat
			       (delete-annotation annotation)
			       (try (log (eval (read-string (subs (:content *ctx*) start end))))
				    (catch Throwable t 
				      (log-exception t) (echo t))))))))))


(defn-log view-dev [events-map]
  (first ; this is the solution for now as there is probably no more than one evaluated expression in each event sent to us     
   (iterate-events events-map "WAVELET_SELF_ADDED" (view-dev-this-blip))))

(defn-log do-replication-by-json [events-map]
  (do-replication @*rep-rules* (incoming-map-to-rep-ops events-map)))

(defn-log allow-gadget-replication [events-map]
  (apply concat (iterate-events events-map "BLIP_SUBMITTED" (handle-gadget-rep))))

(defn concat-apply [fns arg]
  (apply concat 
         (map #(% arg) fns)))

(defn-log view-dev-and-do-replication [events-map]
  (concat-apply 
   [view-dev do-replication-by-json allow-gadget-replication] events-map))


(defn-log crazy-shit [events-map]
  (concat 
   
   (view-dev events-map)
   
   (first ; this is the solution for now as there is probably no more than one evaluated expression in each event sent to us     
    (iterate-events events-map "WAVELET_SELF_ADDED" (view-dev-annotate-blip)))
   
   (first ; this is the solution for now as there is probably no more than one evaluated expression in each event sent to us     
    (iterate-events events-map "BLIP_SUBMITTED" (handle-gadget-rep)))
   
   (do-replication-by-json events-map)))


; ============================
; ======= Server Layer =======
; ============================

(defn answer-wave [events-map]
  (json-str
   (log-info "Operations" (log (wave-attempt
                             (operation-bundle-json ((ns-resolve 'we
                                                                 (read-string
                                                                  ((read-json (events-map "proxyingFor")) "action"))) 
                                                     events-map)))))))




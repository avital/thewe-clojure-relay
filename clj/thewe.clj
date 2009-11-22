
(ns we
 (:use clojure.contrib.json.read)
 (:use clojure.contrib.json.write)
 (:use clojure.set))

; =====================
; ======= Atoms =======
; =====================

(def *rep-rules* (atom #{}))

; =========================
; ======= Utilities =======
; =========================

; (dig {:a {:b 3}} :a :b) returns 3
(defn dig [map & rest]
  (get-in map rest))

(def log-list (atom []))

(defn filter-keys [map keys]
  (into {} (filter #(some #{(key %)} keys) map)))


; =============================
; ======= Logical Layer =======
; =============================

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

(defn-log equal-rep-loc [r1 r2]
  (let [rep-loc-keys [:wave-id :wavelet-id :blip-id]] 
    (= (filter-keys r1 rep-loc-keys) (filter-keys r2 rep-loc-keys))))


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
      [{:rep-loc (assoc basic-rep-loc :type "blip") :content (blip-data "content")}]
      )
    )
  )

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

(defn-log document-delete-append [rep-loc content]
  [(assoc (op-skeleton rep-loc)
     "index"  -1,
      "property"  nil,
      "type"  "DOCUMENT_DELETE")
   (assoc (op-skeleton rep-loc)
     "index"  0,
      "property"  content,
      "type"  "DOCUMENT_APPEND")])

(defn-log document-delete-op [rep-loc]
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

(defn-log delete-annotation-op [rep-loc start end]
  [(assoc (op-skeleton rep-loc)
     "index" -1,
     "property" (range-op-json start end),
     "type" "DOCUMENT_ANNOTATION_DELETE")])

(defn-log add-annotation-op [rep-loc name start end]
  [(assoc (op-skeleton rep-loc)
     "index" -1,
     "property" (annotation-op-json name start end value),
     "type" "DOCUMENT_ANNOTATION_SET")])


(defn-log add-annotation-ops [rep-loc name start end value]
  [(assoc (op-skeleton rep-loc)
     "index" -1,
     "property" (annotation-op-json name start end value),
     "type" "DOCUMENT_ANNOTATION_DELETE")])

; @todo: what is the difference between using "append" and :append?

(defn-log document-insert-op [rep-loc cursor content]
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

(defn-log append-gadget-op [rep-loc gadget-state]
  [(assoc (op-skeleton rep-loc)
     "index" 0,
     "property" (gadget-op-json gadget-state),
     "type" "DOCUMENT_ELEMENT_APPEND")])

(defn-log gadget-submit-delta-ops [rep-loc state]
  [(assoc (op-skeleton rep-loc)
     "index" -1,
     "property" (gadget-op-json state),
     "type" "DOCUMENT_ELEMENT_MODIFY_ATTRS")])

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

(defn-log operation-bundle-json [ops]
  {"javaClass"  "com.google.wave.api.impl.OperationMessageBundle",
   "operations"  {"javaClass"  "java.util.ArrayList",
                  "list"  ops}
   "version"  "106"}) ; @todo deal with version


(defmethod update-rep-loc-ops "gadget" [rep-loc content]
  (gadget-submit-delta-ops rep-loc 
			   {(:key rep-loc) content
			    "url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggb.xml"}))

(defmethod update-rep-loc-ops "blip" [rep-loc content]
  (document-delete-append rep-loc content))

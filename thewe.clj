(ns we
 (:require clojure.contrib.json.read)
 (:require clojure.contrib.json.write)
 (:require clojure.set)
 (:use clojure.contrib.json.read)
 (:use clojure.contrib.json.write)
 (:use clojure.set)
 (:use clojure.contrib.pprint))

; =====================
; ======= Atoms =======
; =====================

(def db (atom {}))
(def rep-rules (atom #{}))

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
  (for [rep-op rep-ops
        rep-class rep-rules :when (some (partial match-rep-loc rep-op) rep-class)
        rep-loc rep-class :when (not= rep-loc (:rep-loc rep-op))]
    {:rep-loc rep-loc :content (:content rep-op)}))




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


; @todo - not nice to have fn here
(defmulti rep-op-to-operations
  (fn [rep-op] {:loc-type (dig rep-op :rep-loc :type) :action (rep-op :action)}))

; nil means replace
(defmethod rep-op-to-operations {:loc-type "blip" :action nil} [rep-op]
  (let [rep-loc (rep-op :rep-loc)
        wave-id (:wave-id rep-loc)
        wavelet-id (:wavelet-id rep-loc)
        blip-id (:blip-id rep-loc) ]
    (if blip-id
      [{"index"  -1,
        "waveletId"  wavelet-id,
        "blipId"  blip-id,
        "javaClass"  "com.google.wave.api.impl.OperationImpl",
        "property"  nil,
        "waveId"  wave-id,
        "type"  "DOCUMENT_DELETE"}
       {"index"  0,
        "waveletId"  wavelet-id,
        "blipId"  blip-id,
        "javaClass"  "com.google.wave.api.impl.OperationImpl",
        "property"  (:content rep-op),
        "waveId"  wave-id,
        "type"  "DOCUMENT_APPEND"
        }]
    [])))

(defmethod rep-op-to-operations {:loc-type "blip" :action "delete"} [rep-op]
  (let [rep-loc (rep-op :rep-loc)
        wave-id (:wave-id rep-loc)
        wavelet-id (:wavelet-id rep-loc)
        blip-id (:blip-id rep-loc) ]
      [{"index"  -1,
        "waveletId"  wavelet-id,
        "blipId"  blip-id,
        "javaClass"  "com.google.wave.api.impl.OperationImpl",
        "property"  nil,
        "waveId"  wave-id,
        "type"  "DOCUMENT_DELETE"}]))

(defmethod rep-op-to-operations {:loc-type "blip" :action "delete-range"} [rep-op]
  (let [rep-loc (rep-op :rep-loc)
        wave-id (:wave-id rep-loc)
        wavelet-id (:wavelet-id rep-loc)
        blip-id (:blip-id rep-loc)]
    `[~@(for [[start end] (:annotate rep-op)] 
	  {"index" -1,
	   "waveletId" wavelet-id,
	   "blipId" blip-id,
	   "javaClass" "com.google.wave.api.impl.OperationImpl",
	   "property" {"javaClass" "com.google.walkabout.api.Range", "end" end, "start" start},
	   "waveId" wave-id,
	   "type" "DOCUMENT_DELETE"})]))


; @todo: what is the difference between using "append" and :append?

(defmethod rep-op-to-operations {:loc-type "blip" :action "insert"} [rep-op]
  (let [rep-loc (rep-op :rep-loc)]
    [{"index"  (:index rep-op),
      "waveletId"  (:wavelet-id rep-loc),
      "blipId"  (:blip-id rep-loc),
      "javaClass"  "com.google.wave.api.impl.OperationImpl",
      "property"  (:content rep-op),
      "waveId"  (:wave-id rep-loc),
      "type"  "DOCUMENT_INSERT"
      }]))

(defmethod rep-op-to-operations {:loc-type "blip" :action "insert-multi"} [rep-op]
  (let [rep-loc (rep-op :rep-loc)]
    `[~@(for [[start end] (:annotate rep-op)] 
	  {"index"  start,
	   "waveletId"  (:wavelet-id rep-loc),
	   "blipId"  (:blip-id rep-loc),
	   "javaClass"  "com.google.wave.api.impl.OperationImpl",
	   "property"  (:content rep-op),
	   "waveId"  (:wave-id rep-loc),
	   "type"  "DOCUMENT_INSERT"})]))

(defmethod rep-op-to-operations {:loc-type "blip" :action "append-gadget"} [rep-op]
  ; @todo: DON'T HAVE THIS DUPLICATION!!! use some sort of basic-rep-op
  (let [rep-loc (rep-op :rep-loc)]
    ; @todo: HORRIBLE
    [{"index" 0,
      "waveletId" (:wavelet-id rep-loc),
      "blipId" (:blip-id rep-loc),
      "javaClass" "com.google.wave.api.impl.OperationImpl",
      "property" {"javaClass" "com.google.wave.api.Gadget",
		  "properties" {"map" (:state rep-op)
				"javaClass" "java.util.HashMap"},
		  "type" "GADGET"},
      "waveId" (:wave-id rep-loc),
      "type" "DOCUMENT_ELEMENT_APPEND"
      }]))

(defmethod rep-op-to-operations {:loc-type "gadget" :action nil} [rep-op]
  (let [rep-loc (rep-op :rep-loc)]
    [{"blipId" (:blip-id rep-loc),
      "index" -1,
      "waveletId" (:wavelet-id rep-loc),
      "javaClass" "com.google.wave.api.impl.OperationImpl",
      "waveId" (:wave-id rep-loc),
      "property" 
      {"type" "GADGET",
       "properties" {"javaClass" "java.util.HashMap",
		     "map" {(:key rep-loc) (:content rep-op)
			    "url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml"}},
       "java_class" "com.google.wave.api.Gadget"},
      "type" "DOCUMENT_ELEMENT_MODIFY_ATTRS"}]))


(defmethod rep-op-to-operations {:loc-type "blip" :action "create-child-blip"} [rep-op]
  (let [rep-loc (rep-op :rep-loc)]
 [{"index" -1,
  "waveletId" (:wavelet-id rep-loc),
  "blipId" (:blip-id rep-loc),
  "javaClass" "com.google.wave.api.impl.OperationImpl",
  "property" {
              "lastModifiedTime" -1,
              "contributors" {"javaClass" "java.util.ArrayList",
                              "list" []},
              "waveletId" (:wavelet-id rep-loc),
              "waveId" (:wave-id rep-loc),
              "parentBlipId" nil,
              "version" -1,
              "creator" nil,
              "content" "",
              "blipId" (:child-blip-id rep-op),
              "javaClass" "com.google.wave.api.impl.BlipData",
              "annotations" {"javaClass" "java.util.ArrayList",
                             "list" []},
              "elements" {"map" {},"javaClass" "java.util.HashMap"},
              "childBlipIds" {"javaClass" "java.util.ArrayList",
                              "list" []}},
  "waveId" (:wave-id rep-loc),
  "type" "BLIP_CREATE_CHILD"}]))


(defn-log wrap-json-operations-with-bundle [list]
  {
   "javaClass"  "com.google.wave.api.impl.OperationMessageBundle",
   "operations"  {
                  "javaClass"  "java.util.ArrayList",
                  "list"  list
                  }
   "version"  "106"   ; @todo WTF
   })

(defn-log rep-ops-to-outgoing-map [rep-ops]
   (wrap-json-operations-with-bundle (mapcat rep-op-to-operations rep-ops)))


; ===================================
; ======= rep-rules utilities =======
; ===================================

(defn rep-by-text! [text]
  (let [rep-class (into #{}
                    (for [[rep-loc content] @db
                          :when content
			  :when (.contains content text)] rep-loc))]
    (swap! rep-rules conj rep-class)
    rep-class))
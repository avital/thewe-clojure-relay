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
(def gadget-db (atom {}))
(def rep-rules (atom #{}))


; =========================
; ======= Utilities =======
; =========================

; (dig {:a {:b 3}} :a :b) returns 3
(defn dig [map & rest]
  (get-in map rest))

(defn log [x]
  (println x)
  x)

; =============================
; ======= Logical Layer =======
; =============================

; Data structures:
; ----------------
;
; rep-loc:    Either {:type "blip" :wave-id wave-id :wavelet-id wavelet-id :blip-id blip-id} or
;                    {:type "gadget" :wave-id wave-id :wavelet-id wavelet-id :blip-id blip-id :key key} or
;
; rep-op:     {:rep-loc rep-loc :content content}
;
; rep-rules:  A set of sets (a partition) of rep-locs


; Receives rep-rules and incoming rep-ops and returns rep-ops to be acted upon
(defn do-replication [rep-rules rep-ops]
  (for [rep-op rep-ops
        rep-class rep-rules :when (contains? rep-class (:rep-loc rep-op))
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

(defn blip-data-to-rep-ops [blip-data]
  (let [basic-rep-loc {:wave-id (blip-data "waveId"), :wavelet-id (blip-data "waveletId"), :blip-id (blip-data "blipId")}]
    (if-let [gadget-map (first (dig blip-data "elements" "map"))]
      ; there is a gadget here
      (let [gadget-state (dig (val gadget-map) "properties" "map")]
        (swap! gadget-db assoc basic-rep-loc gadget-state)
        (for [[k v] gadget-state]
          {:rep-loc (assoc basic-rep-loc :type "gadget" :key k) :content v}))

      ; there is no gadget
      [{:rep-loc (assoc basic-rep-loc :type "blip") :content (blip-data "content")}]
      )
    )
  )


(defn incoming-map-to-rep-ops [incoming]
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
  (fn [- rep-op] {:loc-type (dig rep-op :rep-loc :type) :action (rep-op :action)}))

; nil means replace
(defmethod rep-op-to-operations {:loc-type "blip" :action nil} [gadget-db rep-op]
  (let [rep-loc (rep-op :rep-loc)
        wave-id (:wave-id rep-loc)
        wavelet-id (:wavelet-id rep-loc)
        blip-id (:blip-id rep-loc) ]
      [{
        "index"  -1,
        "waveletId"  wavelet-id,
        "blipId"  blip-id,
        "javaClass"  "com.google.wave.api.impl.OperationImpl",
        "property"  nil,
        "waveId"  wave-id,
        "type"  "DOCUMENT_DELETE"
        }
       {
        "index"  0,
        "waveletId"  wavelet-id,
        "blipId"  blip-id,
        "javaClass"  "com.google.wave.api.impl.OperationImpl",
        "property"  (:content rep-op),
        "waveId"  wave-id,
        "type"  "DOCUMENT_APPEND"
        }]))

; @todo: what is the difference between using "append" and :append?

(defmethod rep-op-to-operations {:loc-type "blip" :action "insert"} [gadget-db rep-op]
  (let [rep-loc (rep-op :rep-loc)]
      [{
        "index"  (:index rep-op),
        "waveletId"  (:wavelet-id rep-loc),
        "blipId"  (:blip-id rep-loc),
        "javaClass"  "com.google.wave.api.impl.OperationImpl",
        "property"  (:content rep-op),
        "waveId"  (:wave-id rep-loc),
        "type"  "DOCUMENT_INSERT"
        }]))

(defmethod rep-op-to-operations {:loc-type "gadget" :action nil} [gadget-db rep-op]
  (let [rep-loc (rep-op :rep-loc)]
      [{
        "index" 1,
        "waveletId" (:wavelet-id rep-loc),
        "blipId" (:blip-id rep-loc),
        "javaClass" "com.google.wave.api.impl.OperationImpl",
        "property" {
                    "javaClass" "com.google.wave.api.Gadget",
                    "properties" {
                                  "map" (assoc 
                                          (gadget-db (dissoc rep-loc :key :type))
                                          (:key rep-loc)
                                          (:content rep-op))
                                  "javaClass" "java.util.HashMap"
                                  },
                    "type" "GADGET"
                    },
        "waveId" (:wave-id rep-loc),
        "type" "DOCUMENT_ELEMENT_REPLACE"
        }]))


(defn rep-ops-to-outgoing-map [gadget-db rep-ops]
  {
   "javaClass"  "com.google.wave.api.impl.OperationMessageBundle",
   "operations"  {
                  "javaClass"  "java.util.ArrayList",
                  "list"  (apply concat
                            (map
                              (partial rep-op-to-operations gadget-db)
                              rep-ops))
                  }
   "version"  "103"   ; @todo WTF
   })



; ===================================
; ======= rep-rules utilities =======
; ===================================

(defn rep-by-text! [text]
  (let [rep-class (into #{}
                    (for [[rep-loc content] @db
                          :when (.contains content text)] rep-loc))]
    (swap! rep-rules conj rep-class)
    rep-class))



; @todo: WHY CAN'T WE REP A GADGET STATE KEY WITH QUOTATION MARKS ("")?


; ====================
; ======= REPL =======
; ====================

(def *current-rep-loc* {})

(defn repl-details [rep-op]
  (let [content (:content rep-op) last-open-index (.lastIndexOf content "[;")]
    (if (= last-open-index -1)
      nil
      (let [last-close-index (.indexOf content ";]" last-open-index)]
        (if (= last-close-index -1)
          nil
          (if (or
                (<= (.length content) (+ last-close-index 2))
                (not= (.charAt content (+ last-close-index 2)) \*))
            {:index (+ last-close-index 2)
             :content (str "*\n" 
                        (binding [*current-rep-loc* (:rep-loc rep-op)] 
                          (eval (read-string (.substring content (+ last-open-index 2) last-close-index))))
                        "\n[;")}
            nil))))))

; @todo: better name?
(defn do-repl [rep-ops]
  (for [rep-op rep-ops
        :let [details (repl-details rep-op)]
        :when details]
    (merge (assoc rep-op :action "insert") details)))










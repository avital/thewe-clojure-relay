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


(def log-list (atom []))

(defn log [x y]
  (println x y)
  (swap! log-list conj [x y])
  y)

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


; @todo: can this be better?
; checks whether the rep-op satisfies the rep-loc definition
(defn match-rep-loc [rep-op rep-loc]
  (if (log "blip-id reploc" (:blip-id rep-loc))
    (= (:rep-loc rep-op) rep-loc)
    (and
      (= (log "d1" (dissoc (:rep-loc rep-op) :blip-id)) (log "d2" (dissoc rep-loc :blip-id :subcontent)))
      (log "contains" (.contains (log "content" (:content rep-op)) (log "subcontent" (:subcontent rep-loc)))))))


; Receives rep-rules and incoming rep-ops and returns rep-ops to be acted upon
(defn do-replication [rep-rules rep-ops]
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
(defmethod rep-op-to-operations {:loc-type "blip" :action nil} [- rep-op]
  (let [rep-loc (rep-op :rep-loc)
        wave-id (:wave-id rep-loc)
        wavelet-id (:wavelet-id rep-loc)
        blip-id (:blip-id rep-loc) ]
    (if blip-id
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
        }]
    [])))

(defmethod rep-op-to-operations {:loc-type "blip" :action "delete"} [- rep-op]
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
        }]))


; @todo: what is the difference between using "append" and :append?

(defmethod rep-op-to-operations {:loc-type "blip" :action "insert"} [- rep-op]
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

(defmethod rep-op-to-operations {:loc-type "gadget" :action nil} [gadget-db- rep-op]
  ; @todo: DON'T HAVE THIS DUPLICATION!!! use some sort of basic-rep-op
  (let [rep-loc (rep-op :rep-loc)]
    ; @todo: HORRIBLE
    (swap! gadget-db assoc (dissoc rep-loc :key :type)
      (assoc (gadget-db- (dissoc rep-loc :key :type)) (:key rep-loc)
        (.replaceAll (.replaceAll (:content rep-op) "<" "&lt;") ">" "&gt;")))
      [{
        "index" 0,
        "waveletId" (:wavelet-id rep-loc),
        "blipId" (:blip-id rep-loc),
        "javaClass" "com.google.wave.api.impl.OperationImpl",
        "property" {
                    "javaClass" "com.google.wave.api.Gadget",
                    "properties" {
                                  "map" (assoc 
                                          (gadget-db- (dissoc rep-loc :key :type))
                                          (:key rep-loc)
                                        ; @todo (url encoding?!)
                                          (.replaceAll (.replaceAll (:content rep-op) "<" "&lt;") ">" "&gt;"))
                                  "javaClass" "java.util.HashMap"
                                  },
                    "type" "GADGET"
                    },
        "waveId" (:wave-id rep-loc),
        "type" "DOCUMENT_ELEMENT_REPLACE"
        }]))

(defmethod rep-op-to-operations {:loc-type "blip" :action "append-gadget"} [- rep-op]
  ; @todo: DON'T HAVE THIS DUPLICATION!!! use some sort of basic-rep-op
  (let [rep-loc (rep-op :rep-loc)]
    ; @todo: HORRIBLE
    (swap! gadget-db assoc (dissoc (:rep-loc rep-op) :type) (:state rep-op))
      [{
        "index" 0,
        "waveletId" (:wavelet-id rep-loc),
        "blipId" (:blip-id rep-loc),
        "javaClass" "com.google.wave.api.impl.OperationImpl",
        "property" {
                    "javaClass" "com.google.wave.api.Gadget",
                    "properties" {
                                  "map" (:state rep-op)
                                  "javaClass" "java.util.HashMap"
                                  },
                    "type" "GADGET"
                    },
        "waveId" (:wave-id rep-loc),
        "type" "DOCUMENT_ELEMENT_APPEND"
        }]))




(defmethod rep-op-to-operations {:loc-type "blip" :action "create-child-blip"} [- rep-op]
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

(defn rep-ops-to-outgoing-map [gadget-db rep-ops]
  {
   "javaClass"  "com.google.wave.api.impl.OperationMessageBundle",
   "operations"  {
                  "javaClass"  "java.util.ArrayList",
                  "list"  (mapcat
                            (partial rep-op-to-operations gadget-db)
                            rep-ops)
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

(def *current-rep-loc*)

(defn view-dev []
  (swap! rep-rules conj 
    #{(assoc *current-rep-loc* :type "gadget" :key "_view.js")
      (dissoc (assoc *current-rep-loc* :subcontent "// js") :blip-id)}
    #{(assoc *current-rep-loc* :type "gadget" :key "_view.html")
      (dissoc (assoc *current-rep-loc* :subcontent "<!-- html -->") :blip-id)}
    #{(assoc *current-rep-loc* :type "gadget" :key "_view.css")
      (dissoc (assoc *current-rep-loc* :subcontent "/* css */") :blip-id)})

  [
   {:rep-loc *current-rep-loc* :action "delete"}
   {:rep-loc *current-rep-loc* :action "append-gadget" :state 
     {"url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml",
      "author" "avital@wavesandbox.com"
      "_view.js" ""
      "_view.html" ""
      "_view.css" ""}}
   {:rep-loc *current-rep-loc* :action "create-child-blip" :child-blip-id "html"}
   {:rep-loc (assoc *current-rep-loc* :blip-id "html") :action "create-child-blip" :child-blip-id "css"}
   {:rep-loc (assoc *current-rep-loc* :blip-id "css") :action "create-child-blip" :child-blip-id "js"}
   {:rep-loc (assoc *current-rep-loc* :blip-id "html") :content "<!-- html -->"}
   {:rep-loc (assoc *current-rep-loc* :blip-id "css") :content "/* css */"}
   {:rep-loc (assoc *current-rep-loc* :blip-id "js") :content "// js"}
  ]
)

(defn repl-outgoing-ops [rep-op]
  (let [content (:content rep-op) last-open-index (.lastIndexOf content "[;")]
    (if (= last-open-index -1)
      []
      (let [last-close-index (.indexOf content ";]" last-open-index)]
        (if (= last-close-index -1)
          []
          (if (or
                (<= (.length content) (+ last-close-index 2))
                (not= (.charAt content (+ last-close-index 2)) \*))
            (binding [*current-rep-loc* (:rep-loc rep-op)]
              ; @todo no need for we/
              (eval (read-string (.substring content (+ last-open-index 2) last-close-index))))
            []))))))

; @todo: better name?
(defn do-repl [rep-ops]
  (mapcat repl-outgoing-ops rep-ops))

(ns we
  (:require clojure.contrib.json.read)
  (:require clojure.contrib.json.write)
  (:require clojure.set)
  (:use clojure.contrib.json.read)
  (:use clojure.contrib.json.write)
  (:use clojure.set)
  (:use compojure))


; =========================
; ======= Utilities =======
; =========================

; (dig {:a {:b 3}} :a :b) returns 3
(defn dig [map & rest]
  (get-in map rest))


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

(defn rep-op-to-operation [rep-op]
  (let [rep-loc (rep-op :rep-loc) wave-id (:wave-id rep-loc) wavelet-id (:wavelet-id rep-loc) blip-id (:blip-id rep-loc) content (:content rep-op)]
  (if (= (rep-loc :type) "blip")
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
      "property"  content,
      "waveId"  wave-id,
      "type"  "DOCUMENT_APPEND"
      }]
    ; else this is a gadget
    [{
      "index" 1,
      "waveletId" wavelet-id,
      "blipId" blip-id,
      "javaClass" "com.google.wave.api.impl.OperationImpl",
      "property" {
                  "javaClass" "com.google.wave.api.Gadget",
                  "properties" {
                                "map" (assoc (@gadget-db (dissoc rep-loc :key :type)) (:key rep-loc) content
                                "javaClass" "java.util.HashMap"
                                },
                  "type" "GADGET"
                  },
      "waveId" wave-id,
      "type" "DOCUMENT_ELEMENT_REPLACE"
      }]))))



(defn rep-ops-to-outgoing-map [rep-ops]
  {
   "javaClass"  "com.google.wave.api.impl.OperationMessageBundle",
   "operations"  {
                  "javaClass"  "java.util.ArrayList",
                  "list"  (apply concat (map rep-op-to-operation rep-ops))
                  }
   "version"  "101"   ; @todo WTF
   })



; ===================================
; ======= rep-rules utilities =======
; ===================================

; @todo: what is db-loc?

(defn rep-by-text! [text]
  (let [rep-class (into #{}
                    (for [[db-loc content] @db
                          :when (.contains content text)] db-loc))]
    (swap! rep-rules conj rep-class)
    rep-class))



; ==========================
; ======= Web Server =======
; ==========================


(defn update-db! [rep-ops]
  (swap! db into
    (for [rep-op rep-ops] [(:rep-loc rep-op) (:content rep-op)])))

(defroutes greeter
  (ANY "/wave"
    (json-str
      (rep-ops-to-outgoing-map
        (do-replication @rep-rules
          (incoming-map-to-rep-ops
            (read-json
              (params :events))))))))


; @TODO WHEN DO WE CALL UPDATE-DB?!$@


(run-server {:port 31337}
  "/*" (servlet greeter))























; TESTS
(def null nil)

(def event-without-gadget
 {"blips"  {
		"map"  {
			"b+2ZbR8dl4D"  {
				"lastModifiedTime"  1254308168017,
				"contributors"  {
					"javaClass"  "java.util.ArrayList",
					"list"  [
						"ayal@wavesandbox.com",
						"thewe-experiments@appspot.com"
					]
				},
				"waveletId"  "wavesandbox.com!conv+root",
				"waveId"  "wavesandbox.com!w+2ZbR8dl4C",
				"parentBlipId"  null,
				"version"  13,
				"creator"  "ayal@wavesandbox.com",
				"content"  " ",
				"blipId"  "b+2ZbR8dl4D",
				"javaClass"  "com.google.wave.api.impl.BlipData",
				"annotations"  {
					"javaClass"  "java.util.ArrayList",
					"list"  [
						{
							"range"  {
								"start"  -1,
								"javaClass"  "com.google.wave.api.Range",
								"end"  1
							},
							"name"  "conv/title",
							"value"  "",
							"javaClass"  "com.google.wave.api.Annotation"
						}
					]
				},
				"elements"  {
					"map"  {},
					"javaClass"  "java.util.HashMap"
				},
				"childBlipIds"  {
					"javaClass"  "java.util.ArrayList",
					"list"  [
					]
				}
			}
		},
		"javaClass"  "java.util.HashMap"
	},
	"robotAddress"  "thewe0@appspot.com",
	"events"  {
		"javaClass"  "java.util.ArrayList",
		"list"  [
			{
				"timestamp"  1254308174642,
				"modifiedBy"  "ayal@wavesandbox.com",
				"javaClass"  "com.google.wave.api.impl.EventData",
				"properties"  {
					"map"  {
						"blipId"  "b+2ZbR8dl4D",
						"participantsRemoved"  {
							"javaClass"  "java.util.ArrayList",
							"list"  [
							]
						},
						"participantsAdded"  {
							"javaClass"  "java.util.ArrayList",
							"list"  [
								"thewe0@appspot.com"
							]
						}
					},
					"javaClass"  "java.util.HashMap"
				},
				"type"  "WAVELET_SELF_ADDED"
			}
		]
	},
	"wavelet"  {
		"lastModifiedTime"  1254308174642,
		"title"  "",
		"waveletId"  "wavesandbox.com!conv+root",
		"rootBlipId"  "b+2ZbR8dl4D",
		"javaClass"  "com.google.wave.api.impl.WaveletData",
		"dataDocuments"  {
			"map"  {
			},
			"javaClass"  "java.util.HashMap"
		},
		"creationTime"  1254308144237,
		"waveId"  "wavesandbox.com!w+2ZbR8dl4C",
		"participants"  {
			"javaClass"  "java.util.ArrayList",
			"list"  [
				"ayal@wavesandbox.com",
				"thewe-experiments@appspot.com",
				"thewe0@appspot.com"
			]
		},
		"creator"  "ayal@wavesandbox.com",
		"version"  15
	}
})


(def event-with-gadget
  {"blips"  {
		"map"  {
			"b+2ZbR8dl4D"  {
				"lastModifiedTime"  1254308168017,
				"contributors"  {
					"javaClass"  "java.util.ArrayList",
					"list"  [
						"ayal@wavesandbox.com",
						"thewe-experiments@appspot.com"
					]
				},
				"waveletId"  "wavesandbox.com!conv+root",
				"waveId"  "wavesandbox.com!w+2ZbR8dl4C",
				"parentBlipId"  null,
				"version"  13,
				"creator"  "ayal@wavesandbox.com",
				"content"  " ",
				"blipId"  "b+2ZbR8dl4D",
				"javaClass"  "com.google.wave.api.impl.BlipData",
				"annotations"  {
					"javaClass"  "java.util.ArrayList",
					"list"  [
						{
							"range"  {
								"start"  -1,
								"javaClass"  "com.google.wave.api.Range",
								"end"  1
							},
							"name"  "conv/title",
							"value"  "",
							"javaClass"  "com.google.wave.api.Annotation"
						}
					]
				},
				"elements"  {
					"map"  {
						"0"  {
							"javaClass"  "com.google.wave.api.Gadget",
							"properties"  {
								"map"  {
									"_view"  "stateUpdated = function(){};",
									"KEY"  "XXX",
									"url"  "http //wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml"
								},
								"javaClass"  "java.util.HashMap"
							},
							"type"  "GADGET"
						}
					},
					"javaClass"  "java.util.HashMap"
				},
				"childBlipIds"  {
					"javaClass"  "java.util.ArrayList",
					"list"  [
					]
				}
			}
		},
		"javaClass"  "java.util.HashMap"
	},
	"robotAddress"  "thewe0@appspot.com",
	"events"  {
		"javaClass"  "java.util.ArrayList",
		"list"  [
			{
				"timestamp"  1254308174642,
				"modifiedBy"  "ayal@wavesandbox.com",
				"javaClass"  "com.google.wave.api.impl.EventData",
				"properties"  {
					"map"  {
						"blipId"  "b+2ZbR8dl4D",
						"participantsRemoved"  {
							"javaClass"  "java.util.ArrayList",
							"list"  [
							]
						},
						"participantsAdded"  {
							"javaClass"  "java.util.ArrayList",
							"list"  [
								"thewe0@appspot.com"
							]
						}
					},
					"javaClass"  "java.util.HashMap"
				},
				"type"  "WAVELET_SELF_ADDED"
			}
		]
	},
	"wavelet"  {
		"lastModifiedTime"  1254308174642,
		"title"  "",
		"waveletId"  "wavesandbox.com!conv+root",
		"rootBlipId"  "b+2ZbR8dl4D",
		"javaClass"  "com.google.wave.api.impl.WaveletData",
		"dataDocuments"  {
			"map"  {
			},
			"javaClass"  "java.util.HashMap"
		},
		"creationTime"  1254308144237,
		"waveId"  "wavesandbox.com!w+2ZbR8dl4C",
		"participants"  {
			"javaClass"  "java.util.ArrayList",
			"list"  [
				"ayal@wavesandbox.com",
				"thewe-experiments@appspot.com",
				"thewe0@appspot.com"
			]
		},
		"creator"  "ayal@wavesandbox.com",
		"version"  15
	}
})

(def blip-data-with-gadget (val (first (get-in event-with-gadget ["blips" "map"]))))

(def blip-data-without-gadget (val (first (get-in event-without-gadget ["blips" "map"]))))

(defn test-blip-data-to-db-structure-with-gadget []
    (blip-data-to-db-structure "12345" blip-data-with-gadget))

(defn test-blip-data-to-db-structure-without-gadget []
    (blip-data-to-db-structure "12345" blip-data-without-gadget))


(reset! sync-table #{})

(def events1
  {"blips"
   {"map"
    {"b+MfdUDFjy%F"
     {"lastModifiedTime" 1252401883999,"contributors"
      {"javaClass" "java.util.ArrayList","list"
       ["avital@wavesandbox.com","thewe-experiments@appspot.com","thewe-0@appspot.com","thewe-operator@appspot.com"]
      },
      "waveletId" "wavesandbox.com!conv+root",
      "waveId" "wavesandbox.com!w+MfdUDFjy%E",
      "parentBlipId" nil,
      "version" 125,
      "creator" "avital@wavesandbox.com",
      "content" "hi hi hi\n","blipId" "b+MfdUDFjy%F","javaClass" "com.google.wave.api.impl.BlipData","annotations" {"javaClass" "java.util.ArrayList","list" []},"elements" {"map" {"0" {"javaClass" "com.google.wave.api.Gadget","properties" {"map" {"test" "jjkklll;qqpopslKL\n","_view" "stateUpdated = function(){};","url" "http //wave.thewe.net/gadgets/theWE-wave/theWE-container.xml","_gadget-id" "http //wave.thewe.net/gadgets/theWE-wave/theWE-container.xml0.607755900749904"},"javaClass" "java.util.HashMap"},"type" "GADGET"}},"javaClass" "java.util.HashMap"},"childBlipIds" {"javaClass" "java.util.ArrayList","list" []}}},"javaClass" "java.util.HashMap"},"events" {"javaClass" "java.util.ArrayList","list" [{"timestamp" 1252402678257,"modifiedBy" "avital@wavesandbox.com","javaClass" "com.google.wave.api.impl.EventData","properties" {"map" {"blipId" "b+MfdUDFjy%F"},"javaClass" "java.util.HashMap"},"type" "DOCUMENT_CHANGED"}]},"wavelet" {"lastModifiedTime" 1252402678257,"title" "","waveletId" "wavesandbox.com!conv+root","rootBlipId" "b+MfdUDFjy%F","javaClass" "com.google.wave.api.impl.WaveletData","dataDocuments" {"map" {},"javaClass" "java.util.HashMap"},"creationTime" 1252400711330,"waveId" "wavesandbox.com!w+MfdUDFjy%E","participants" {"javaClass" "java.util.ArrayList","list" ["avital@wavesandbox.com","thewe-experiments@appspot.com","thewe-0@appspot.com","thewe-operator@appspot.com"]},"creator" "avital@wavesandbox.com","version" 129}
   }
)

(def sync-table1 #{#{{:wave-id "wavesandbox.com!w+MfdUDFjy%E" :wavelet-id "wavesandbox.com!conv+root" :blip-id "b+MfdUDFjy%F"}
                     {:wave-id "a" :wavelet-id "b" :blip-id "c"}}})

(update-db sync-table1 events1)






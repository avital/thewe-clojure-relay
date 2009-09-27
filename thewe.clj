(ns we
  (:require clojure.contrib.json.read)
  (:require clojure.contrib.json.write)
  (:require clojure.set)
  (:use clojure.contrib.json.read)
  (:use clojure.contrib.json.write)
  (:use clojure.set)
  (:use compojure))

; db is a map between replocs and blipData (= the event json we get from wave)
(def db (atom {}))
(def sync-table (atom #{}))
(def log-buffer (atom ""))


;(defmacro defn [name- args & body]
;  `(defn ~name- ~args
;    (let [return-value# (do ~@body)]
;      return-value#)))

(defn log [& what]
;  (if (coll? (second what)) (do (print (count what)) doall (second what)))
;  (try (doall (second what))) ;@todo
;  (println what)
;  (swap! log-buffer str what "\n\n")
  (last what))

(defn substring [a b]
  (.substring a b))

(defn read-and-clean-log []
  (let [old-buffer @log-buffer]
    (swap! log-buffer substring (count old-buffer))
    old-buffer))

(defmacro forex [seq-exprs & body-expr]
  `(apply concat (for ~seq-exprs (list ~@body-expr))))

(defn pairs-to-map [pairs]
  (zipmap (map first pairs) (map second pairs)))

(defn map-map [f m]
  (pairs-to-map (map #(f (first %) (second %)) m)))

(defn blip-data-to-db-structure [blip-id blip-data]
  [{:wave-id (blip-data "waveId"), :wavelet-id (blip-data "waveletId"), :blip-id blip-id} blip-data])

(defn disj1 [set x]
  (if set
    (disj set x)
    nil))

; updates has the same structure as db (see db comment)
; returns a map from reploc to its new content
(defn updates-to-log-opers [sync-table updates]
  (into {} (for [update updates
        sync-class sync-table :when (contains? sync-class (key update))
        reploc sync-class :when (not= reploc (key update))]
    [reploc ((val update) "content")])))


(def log-reploc {:wave-id "wavesandbox.com!w+0sqEDj2G%A"
                 :wavelet-id "wavesandbox.com!conv+root"
                 :blip-id "b+0sqEDj2G%B"})

(defn log-opers-to-json [log-opers]
  {
	"javaClass"  "com.google.wave.api.impl.OperationMessageBundle",
	"operations"  {
		"javaClass"  "java.util.ArrayList",
		"list"  (conj (log "Operations:" (forex [log-oper log-opers :let [reploc (key log-oper)]]
			{
				"index"  -1,
				"waveletId"  (:wavelet-id reploc),
				"blipId"  (:blip-id reploc),
				"javaClass"  "com.google.wave.api.impl.OperationImpl",
				"property"  nil,
				"waveId"  (:wave-id reploc),
				"type"  "DOCUMENT_DELETE"
			}
			{
				"index"  0,
				"waveletId"  (:wavelet-id reploc),
				"blipId"  (:blip-id reploc),
				"javaClass"  "com.google.wave.api.impl.OperationImpl",
				"property"  (val log-oper),
				"waveId"  (:wave-id reploc),
				"type"  "DOCUMENT_APPEND"
			}))
                        {
				"index"  0,
				"waveletId"  (:wavelet-id log-reploc),
				"blipId"  (:blip-id log-reploc),
				"javaClass"  "com.google.wave.api.impl.OperationImpl",
				"property"  (read-and-clean-log),
				"waveId"  (:wave-id log-reploc),
				"type"  "DOCUMENT_APPEND"
                         ; @todo MAKE THESE FUNCTIONS
                        })
            }
	"version"  "19"   ; @todo WTF
})

(defn containsex? [big small]
  (some #{small} big))

(defn update-db [sync-table data]
  (let [db-updates (into {} (for [[blip-id blip-data] (get-in data ["blips" "map"])
                                  :let [db-str (blip-data-to-db-structure blip-id blip-data)]
                                  :when (not= (first db-str) log-reploc)] db-str))]
    (swap! db merge db-updates)
    (json-str (log-opers-to-json (updates-to-log-opers sync-table
                                   (into {} (for [db-update db-updates
                                                  :when (containsex?
                                                          (for [event (get-in data ["events" "list"])
                                                                :when (not= (event "modifiedBy") "panda@gwave.com")]
                                                            (get-in event ["properties" "map" "blipId"]))
                                                          ((key db-update) :blip-id))] db-update))))))) ; @todo forex not force db-update at end


(defn sync-with-text [text]
  (let [sync-class (disj
                           (set (keys (filter (fn [[key val]] (.contains (val "content") text)) @db)))
                           log-reploc)]
    (swap! sync-table conj sync-class)
    sync-class))


(defroutes greeter
  (ANY "/wave"
    (let [data (log "Events: " (read-json (params :events)))]
      (update-db @sync-table data))))

(run-server {:port 31337}
  "/*" (servlet greeter))






; TESTS

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
      "content" "hi hi hi\n","blipId" "b+MfdUDFjy%F","javaClass" "com.google.wave.api.impl.BlipData","annotations" {"javaClass" "java.util.ArrayList","list" []},"elements" {"map" {"0" {"javaClass" "com.google.wave.api.Gadget","properties" {"map" {"test" "jjkklll;qqpopslKL\n","_view" "stateUpdated = function(){};","url" "http //wave.thewe.net/gadgets/theWE-wave/theWE-container.xml","_gadget-id" "http //wave.thewe.net/gadgets/theWE-wave/theWE-container.xml0.607755900749904"},"javaClass" "java.util.HashMap"},"type" "GADGET"}},"javaClass" "java.util.HashMap"},"childBlipIds" {"javaClass" "java.util.ArrayList","list" []}}},"javaClass" "java.util.HashMap"},"events" {"javaClass" "java.util.ArrayList","list" [{"timestamp" 1252402678257,"modifiedBy" "avital@wavesandbox.com","javaClass" "com.google.wave.api.impl.EventData","properties" {"map" {"blipId" "b+MfdUDFjy%F"},"javaClass" "java.util.HashMap"},"type" "DOCUMENT_CHANGED"}]},"wavelet" {"lastModifiedTime" 1252402678257,"title" "","waveletId" "wavesandbox.com!conv+root","rootBlipId" "b+MfdUDFjy%F","javaClass" "com.google.wave.api.impl.WaveletData","dataDocuments" {"map" {},"javaClass" "java.util.HashMap"},"creationTime" 1252400711330,"waveId" "wavesandbox.com!w+MfdUDFjy%E","participants" {"javaClass" "java.util.ArrayList","list" ["avital@wavesandbox.com","thewe-experiments@appspot.com","thewe-0@appspot.com","thewe-operator@appspot.com"]},"creator" "avital@wavesandbox.com","version" 129}}
)

(def sync-table1 #{#{{:wave-id "wavesandbox.com!w+MfdUDFjy%E" :wavelet-id "wavesandbox.com!conv+root" :blip-id "b+MfdUDFjy%F"}
                     {:wave-id "a" :wavelet-id "b" :blip-id "c"}}})

(update-db sync-table1 events1)






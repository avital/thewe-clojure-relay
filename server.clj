(ns we
 (:use compojure))

(defn update-db! [rep-ops]
  (swap! db into
    (for [rep-op rep-ops] [(:rep-loc rep-op) (:content rep-op)])))

(defn-log answer-wave [events-map]
  (json-str
    (rep-ops-to-outgoing-map
      ((ns-resolve 'we
         (read-string
           (events-map "proxyingFor"))) events-map))))

(defroutes server
  (GET "/log"
    (json-str @*call-log*))
  (ANY "/wave"
    (answer-wave (read-json (params :events)))))

(defmacro iterate-events [events listen-to for-args]
  `(let [~'modified-blip-ids
	 (for [~'event (dig ~events "events" "list")
	       :when (not (.endsWith (~'event "modifiedBy") "@a.gwave.com"))
	       :when (= (~'event "type") ~listen-to)]
	   (dig ~'event "properties" "map" "blipId"))]
     (for [~'blip-id ~'modified-blip-ids
	   :let [~'blip-data (dig ~events "blips" "map" ~'blip-id)
		 ~'rep-loc (assoc {:type "blip"} :wave-id (~'blip-data "waveId") :wavelet-id (~'blip-data "waveletId") :blip-id (~'blip-data "blipId"))
		 ~'rep-op {:rep-loc ~'rep-loc :content (~'blip-data "content")}
		 ~'first-gadget-map (first (dig ~'blip-data "elements" "map"))
		 ~'gadget-state (if ~'first-gadget-map (dig (val ~'first-gadget-map) "properties" "map") {})]]
 ~for-args )))


(defn-log identify-this-blip [rep-op rep-loc gadget-state] 
  [(assoc rep-op
      :action nil
      :loc-type "blip"
      :content (str rep-loc))])

(defn-log identify-blip [events-map]
  (apply concat 
	(iterate-events events-map "WAVELET_SELF_ADDED" (identify-this-blip rep-op rep-loc gadget-state))))

(defn-log create-child-blip [rep-op rep-loc gadget-state] 
  [(assoc rep-op
      :action "create-child-blip"
      :loc-type "blip"
      :child-blip-id "new-blip-id"
      :content (str "hi!"))])

(defn run-function-do-operations [events-map]
  (apply concat (iterate-events events-map "BLIP_SUBMITTED" (if-let [func-name (:content rep-op)]
		  ((ns-resolve 'we
			       (read-string func-name)) rep-op rep-loc nil)))))

;this has swap! here -  is there a way to prevent it?
(defn view-dev-this-blip [rep-op rep-loc gadget-state]
  (swap! rep-rules conj
    #{(assoc rep-loc :type "gadget" :key "_view.js")
      (dissoc (assoc rep-loc :subcontent "// js") :blip-id)}
    #{(assoc rep-loc :type "gadget" :key "_view.html")
      (dissoc (assoc rep-loc :subcontent "<!-- html -->") :blip-id)}
    #{(assoc rep-loc :type "gadget" :key "_view.css")
      (dissoc (assoc rep-loc :subcontent "/* css */") :blip-id)})

  [{:rep-loc rep-loc :action "delete"}
   {:rep-loc rep-loc :action "append-gadget" :state
    {"url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml",
     "author" "avital@wavesandbox.com"
     "_view.js" ""
     "_view.html" ""
     "_view.css" ""}}
   {:rep-loc rep-loc :action "create-child-blip" :child-blip-id "html"}
   {:rep-loc (assoc rep-loc :blip-id "html") :action "create-child-blip" :child-blip-id "css"}
   {:rep-loc (assoc rep-loc :blip-id "css") :action "create-child-blip" :child-blip-id "js"}
   {:rep-loc (assoc rep-loc :blip-id "html") :content "<!-- html -->"}
   {:rep-loc (assoc rep-loc :blip-id "css") :content "/* css */"}
   {:rep-loc (assoc rep-loc :blip-id "js") :content "// js"}])

(defn-log view-dev [events-map]
  (apply concat
	 (iterate-events events-map "WAVELET_SELF_ADDED" (view-dev-this-blip rep-op rep-loc gadget-state))))

(defn do-replication-by-json [events-map]
  (let [rep-ops (incoming-map-to-rep-ops
                  events-map)]
    (update-db! rep-ops)
        (concat
          (do-replication @rep-rules rep-ops))))

(defn-log view-dev-and-do-replication [events-map] 
  (concat (view-dev events-map) (do-replication-by-json events-map) ))


(run-server {:port 31337}
  "/*" (servlet server))

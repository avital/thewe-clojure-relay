(ns we
  (:use clojure.contrib.duck-streams)
  (:use compojure)
  (:use clojure.set)
  (:import java.util.Date))

(defn current-time []
  (. (new Date) (toString)))

(defmacro wave-attempt [expr]
  `(try ~expr 
	(catch Throwable t#
	  (append-spit "/home/avital/swank/log/exceptions"
		       (str (current-time) \newline 
			    t# \newline \newline)))))

(defn log-info [title x]
  (append-spit "/home/avital/swank/log/events"
	       (str (current-time) \newline title \newline (pprn-str x) \newline))
  x)

(defn-log answer-wave [events-map]
  (json-str
   (log-info "Operations" (wave-attempt
      (rep-ops-to-outgoing-map
       ((ns-resolve 'we
		    (read-string
		     ((read-json (events-map "proxyingFor")) "action"))) 
	(log-info "Events" events-map)))))))

(def js-snippet 
     "modeChanged = function(lastMode, newMode) {
\tif (lastMode == wave.Mode.EDIT) {
\t\twe.state.set('value', $('edit').get('value'));
\t}

\t// Here are the numeric values of the different modes: {UNKNOWN:0, VIEW:1, EDIT:2, DIFF_ON_OPEN:3, PLAYBACK:4};
\t// An array that associates for each mode the element that should be displayed
\tvar viewsByMode = [$('view'), $('view'), $('edit'), $('view'), $('view')]

\tviewsByMode.each(function(el) {
\t\tel.setStyle('display', 'none')
\t})

\tviewsByMode[newMode].setStyle('display', 'inline')
}")

(defroutes server
  (GET "/tests/start"
    (def *record-unit-tests* true)
    "Ok")
  (GET "/tests/stop"
    (def *record-unit-tests* false)
    (approve-tests)
    "Done")
  (GET "/log/start"
    (reset! *call-log* {})
    (def *enable-logging* true)
    "Hi")
  (GET "/log/stop" 
    [(comment {:headers {"Content-Type" "text/plain"}})
     (do
       (def *enable-logging* false)
       (str "<html><head></head><body><span id='redirect'>"
	    (escape-html (json-str @*call-log*))
	    "</span><script type='text/javascript'>window.location ="
	    "'http://thewe.net/json-tree#' + "
	    "document.getElementById('redirect').textContent"
	    "</script></body></html>"))])
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
		 ~'content (~'blip-data "content")		 
		 ~'blip-annotations (dig ~'blip-data "annotations" "list")		 
		 ~'annotated-range
		 (for [~'annotation ~'blip-annotations 
		       :when (= (~'annotation "name") "we/eval")
 		       :when (not= -1 (dig ~'annotation "range" "start"))]
		   [(dig ~'annotation "range" "start") (dig ~'annotation "range" "end")])

		 ~'rep-loc {:type "blip"  :wave-id (~'blip-data "waveId") :wavelet-id (~'blip-data "waveletId") :blip-id (~'blip-data "blipId")}
		 ~'rep-op {:rep-loc ~'rep-loc :content ~'content  :annotate ~'annotated-range}		 

		 ~'first-gadget-map (first (dig ~'blip-data "elements" "map"))
		 ~'gadget-state (if ~'first-gadget-map (dig (val ~'first-gadget-map) "properties" "map") {})]] ~for-args )))


(defn-log identify-this-blip [rep-op rep-loc gadget-state] 
  [(assoc rep-op
     :action "delete-range"
     :loc-type "blip")
   (assoc rep-op
      :action "insert-multi"
      :loc-type "blip"
      :content (str rep-loc))])

(defn-log identify-blip [events-map]
  (apply concat 
	(iterate-events events-map "WAVELET_SELF_ADDED" (identify-this-blip rep-op rep-loc gadget-state))))

(defn-log create-child-blip [rep-op rep-loc gadget-state] 
  [(assoc rep-op
     :action "delete-range"
     :loc-type "blip")
   (assoc rep-op
      :action "create-child-blip"
      :loc-type "blip"
      :child-blip-id "new-blip-id"
      :content (str "hi!"))])

(defn burp-html [rep-op rep-loc gadget-state]
  [(assoc rep-op
     :action "delete-range"
     :loc-type "blip")
   (assoc rep-op
     :action "insert-multi"
     :loc-type "blip"
     :content "<span wecursor='value'>
<span id='view' wethis=1></span>
<input id='edit' wethis=1></input>
</span>")])

(defn-log run-function-do-operations [events-map]
  (apply concat
	 (iterate-events events-map "DOCUMENT_CHANGED"     
			 (apply concat 
				(for [[start end] annotated-range] 
				  (if-let [func-to-run 
					   (eval (read-string (subs (:content rep-op) start end)))]  
				    (func-to-run rep-op rep-loc gadget-state)))))))

(def *clipboard* (atom nil))

(defn-log remember-gadget-key! [rep-key]
  (fn [rep-op rep-loc gadget-state]
    (reset! *clipboard* 
	    {:source-key rep-key 
	     :rep-loc rep-loc 
	     :subkeys (for [key (keys gadget-state) 
			     :when (or (= key rep-key) (.startsWith key (str rep-key ".")))]
			 (.replace key rep-key ""))})
    [(assoc rep-op
       :action "delete-range"
       :loc-type "blip")]))

(defn-log containing-rep-class [rep-loc]
  (first (for [rep-class @rep-rules :when (some #{rep-loc} rep-class)] rep-class)))

(defn-log add-to-class [partition class el] 
  (conj (disj partition class) (conj class el)))

(defn-log replicate-replocs! [r1 r2]
  (let [rc1 (containing-rep-class r1) rc2 (containing-rep-class r2)]
    (cond
      (and (not rc1) (not rc2))  ; when both are not in rep-classes
      (swap! rep-rules conj #{r1 r2})

      (and rc1 (not rc2))
      (swap! rep-rules add-to-class rc1 r2)

      (and rc2 (not rc1))
      (swap! rep-rules add-to-class rc2 r1)

      (and (and rc1 rc2) (not= rc1 rc2))
      (swap! rep-rules #(conj (disj % rc1 rc2) (union rc1 rc2))))))


(defn-log replicate-gadget-key! [rep-key]
  (fn [rep-op rep-loc gadget-state]
    (doseq [:let [{subkeys :subkeys source-key :source-key source-rep-loc :rep-loc} @*clipboard*] subkey subkeys] 
      (replicate-replocs!     
       (assoc source-rep-loc :type "gadget" :key (str source-key subkey))
       (assoc rep-loc :type "gadget" :key (str rep-key subkey))))
    [(assoc rep-op
       :action "delete-range"
       :loc-type "blip")]))

(defn append-text [s]
  (fn [rep-op _ _] 
    [(assoc rep-op
       :action "delete-range"
       :loc-type "blip")
     (assoc rep-op
       :action "insert-multi"
       :loc-type "blip"
       :content s)]))

(defn burp-js [] (append-text js-snippet))

(defn create-view-dev-replication [_ rep-loc _]
  (swap! rep-rules conj
	 #{(assoc rep-loc :type "gadget" :key "_view.js")
	   (dissoc (assoc rep-loc :subcontent "// js") :blip-id)}
	 #{(assoc rep-loc :type "gadget" :key "_view.html")
	   (dissoc (assoc rep-loc :subcontent "<!-- html -->") :blip-id)}
	 #{(assoc rep-loc :type "gadget" :key "_view.css")
	   (dissoc (assoc rep-loc :subcontent "/* css */") :blip-id)})
  [])

; @TODO this has swap! here -  is there a way to prevent it?
(defn view-dev-this-blip [_ rep-loc _] 
  (create-view-dev-replication _ rep-loc _)
  [{:rep-loc rep-loc :action "delete"}
   {:rep-loc rep-loc :action "append-gadget" :state
    {"url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml",
     "author" "avital@wavesandbox.com"
     "_view.js" ""
     "_view.html" ""
     "_view.css" ""
     "_waveId" (rep-loc :wave-id)
     "_waveletId" (rep-loc :wavelet-id)
     "_blipId" (rep-loc :blip-id)
     }}
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
  (do-replication @rep-rules (incoming-map-to-rep-ops events-map)))

(defn-log view-dev-and-do-replication [events-map] 
  (concat (view-dev events-map) (do-replication-by-json events-map)))




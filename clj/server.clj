(ns we
  (:use clojure.contrib.core)
  (:use clojure.contrib.duck-streams)
  (:use compojure)
  (:use clojure.set)
  (:import java.util.Date))

(defn current-time []
  (. (new Date) (toString)))

(defn log-exception [t]
  (append-spit "/home/avital/swank/log/exceptions"
	       (str (current-time) \newline 
		    t \newline \newline)))

(defmacro wave-attempt [expr]
  `(try ~expr 
	(catch Throwable t#
	  (log-exception t#)
	  (wrap-json-operations-with-bundle []))))

(defn-log log-info [title x]
  (append-spit "/home/avital/swank/log/operations"
	       (str (current-time) \newline title \newline (pprn-str x) \newline))
  x)

(defn-log answer-wave [events-map]
  (json-str
   (log-info "Operations" (wave-attempt
			   ((ns-resolve 'we
					(read-string
					 ((read-json (events-map "proxyingFor")) "action"))) 
			    events-map)))))

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

; @todo - directory structure?
(def json-tree-html (slurp "/home/avital/swank/assets/json-tree.html"))

(def html-snippet "<span wecursor='value'>\t<span id='view' wethis=1></span>\t<input id='edit' wethis=1></input></span>")

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
    "Logging is currently activated. Do some stuff in wave and when you are done, <a href='../log/stop'>stop logging and see results</a>")
  (GET "/log/stop" 
    [(comment {:headers {"Content-Type" "text/plain"}})
     (do
       (def *enable-logging* false)
       (.replace json-tree-html "@@@result@@@" (escape-html (json-str @*call-log*))))])
  (ANY "/wave"
    (log (let [events (log (params :events))]
	   (if (or (.contains events "\"name\":\"we/eval\"")
		   (.contains events "BLIP_SUBMITTED")
		   (.contains events "WAVELET_SELF_ADDED"))
	     (answer-wave (read-json (params :events)))
	     (json-str (wrap-json-operations-with-bundle [])))))))


(def *event-context*)

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
		 ~'gadget-state (if ~'first-gadget-map (dig (val ~'first-gadget-map) "properties" "map") {})
		 ~'rep-op {:rep-loc ~'rep-loc :content ~'content :annotations ~'blip-annotations :gadget-state ~'gadget-state}]] 
       (binding [~'*event-context* ~'rep-op] ~for-args))))


(defn-log delete-annotation [annotation]
  (mapcat rep-op-to-operations  
	  [(assoc *event-context*
	     :action "delete-annotation"
	     :loc-type "blip"
	     :range (log (annotation "range")))]))

(defn sfirst [x]
  (if-let [result (first x)]
    result
    []))

(defn-log run-function-do-operations [events-map] ; this is the signature of a function that can be called by adding + to a robot's address
  (wrap-json-operations-with-bundle
    (sfirst ; this is the solution for now as there is probably no more than one evaluated expression in each event sent to us
     (iterate-events events-map "DOCUMENT_CHANGED"     
		     (apply concat 
			    (for [annotation (log (:annotations rep-op)) 		  
				  :when (not= -1 (dig annotation "range" "start"))
				  :when (= "we/eval" (annotation "name"))
				  :let [start (dig annotation "range" "start") 
					end (dig annotation "range" "end")]] 
			      (binding [*event-context* (assoc *event-context* :cursor end)]
			       (concat
				(delete-annotation annotation)
				(try (eval (read-string (subs (:content rep-op) start end)))
				     (catch Throwable t 
				       (log-exception t) (echo t)))))))))))



(def foo run-function-do-operations)

(defn-log echo-pp [s]
  (echo (pprn-str s)))

(defn-log echo [s]
  (mapcat rep-op-to-operations 
	  [(assoc *event-context*
	     :action "insert"
	     :loc-type "blip"
	     :content  (str \newline s))]))

(defn-log identify-this-blip []
  (echo-pp (:rep-loc *event-context*)))

(defn-log create-child-blip [] 
  (mapcat rep-op-to-operations  
	  [(assoc *event-context*
	     :action "create-child-blip"
	     :loc-type "blip"
	     :child-blip-id "new-blip-id"
	     :content (str "hi!"))]))

(defn burp-js [] (echo js-snippet))
(defn burp-html [] (echo html-snippet))

(def *clipboard* (atom nil))
(def *other-wave* (atom nil))

(defn-log remember-wave! []
  (reset! *other-wave* *event-context*)
  (echo "Remembered"))

(defmacro on-other-wave [expr]
  `(binding [*event-context* @*other-wave*]
     ~expr))

(defn-log modify-ggg [key val]
  [{"blipId" (dig *event-context* :rep-loc :blip-id),
    "index" -1,
    "waveletId" (dig *event-context* :rep-loc :wavelet-id),
    "javaClass" "com.google.wave.api.impl.OperationImpl",
    "waveId" (dig *event-context* :rep-loc :wave-id),
    "property"
    {"type" "GADGET",
     "properties"
     {"javaClass" "java.util.HashMap",
      "map"
      {key val,
       "url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggg.xml"}},
     "javaClass" "com.google.wave.api.Gadget"},
    "type" "DOCUMENT_ELEMENT_MODIFY_ATTRS"}])

(defn-log remember-gadget-key! [rep-key]
  (reset! *clipboard* 
	  {:source-key rep-key 
	   :rep-loc (:rep-loc *event-context*) 
	   :subkeys (for [key (keys (:gadget-state *event-context*)) 
			  :when (or (= key rep-key) (.startsWith key (str rep-key ".")))]
		      (.replaceFirst key rep-key ""))}) ; this never happened
  (echo "ok!"))

(defn-log containing-rep-class [rep-loc]
  (first (for [rep-class @rep-rules :when (some #{rep-loc} rep-class)] rep-class)))

(defn-log current-rep-class []
  (containing-rep-class (*event-context* :rep-loc)))

(defn-log gadget-rep-class [key]
  (containing-rep-class (assoc (*event-context* :rep-loc)
			   :type "gadget" :key key)))


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
  (doseq [:let [{subkeys :subkeys source-key :source-key source-rep-loc :rep-loc} @*clipboard*] subkey subkeys] 
    (replicate-replocs!     
     (assoc source-rep-loc :type "gadget" :key (str source-key subkey))
     (assoc (:rep-loc *event-context*) :type "gadget" :key (str rep-key subkey))))
  (echo "replicated!"))

(defn-log create-view-dev-replication! []
  (let [rep-loc (:rep-loc *event-context*)]
    (replicate-replocs!
     (assoc rep-loc :type "gadget" :key "_view.js")
     (dissoc (assoc rep-loc :subcontent "// js") :blip-id))
	
    (replicate-replocs!
     (assoc rep-loc :type "gadget" :key "_view.html")
     (dissoc (assoc rep-loc :subcontent "<!-- html -->") :blip-id))
	
    (replicate-replocs!
     (assoc rep-loc :type "gadget" :key "_view.css")
     (dissoc (assoc rep-loc :subcontent "/* css */") :blip-id)))
  
  (mapcat rep-op-to-operations []))

; @TODO this has swap! here -  is there a way to prevent it?
(defn view-dev-this-blip []
  (create-view-dev-replication!)
  (let [rep-loc (:rep-loc *event-context*)]
    (mapcat rep-op-to-operations   
	    [{:rep-loc rep-loc :action "delete"}
	     {:rep-loc rep-loc :action "append-gadget" :state
	      {"url" "http://wave.thewe.net/gadgets/thewe-ggg/thewe-ggb.xml",
	       "author" "avital@wavesandbox.com"
	       "_view.js" ""
	       "_view.html" ""
	       "_view.css" ""
	       "_rep-loc.waveId" (rep-loc :wave-id)
	       "_rep-loc.waveletId" (rep-loc :wavelet-id)
	       "_rep-loc.blipId" (rep-loc :blip-id)}}
	     {:rep-loc rep-loc :action "create-child-blip" :child-blip-id "html"}
	     {:rep-loc (assoc rep-loc :blip-id "html") :action "create-child-blip" :child-blip-id "css"}
	     {:rep-loc (assoc rep-loc :blip-id "css") :action "create-child-blip" :child-blip-id "js"}
	     {:rep-loc (assoc rep-loc :blip-id "html") :content "<!-- html -->"}
	     {:rep-loc (assoc rep-loc :blip-id "css") :content "/* css */"}
	     {:rep-loc (assoc rep-loc :blip-id "js") :content "// js"}])))

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

(def op-map-path ["property" "properties" "map"])

(defn-log unite-gadget-chunk [ops]
  (assoc-in (first ops) op-map-path
	    (apply merge (for [op ops] (get-in op op-map-path)))))

(defn-log op-skeleton [op]
  (dissoc-in op op-map-path))

(defn-log partition-by-func [coll f]
  (let [skeletons (into #{} (map f coll))]
    (for [skeleton skeletons]
      (for [x coll :when (= (f x) skeleton)] x))))

(defn-log find-gadget-chunks [ops]
  (partition-by-func ops op-skeleton))

(defn-log unite-gadget-modifications [ops]
  (for [chunk (find-gadget-chunks ops)] (unite-gadget-chunk chunk)))

(defn-log view-dev [events-map]
  (first ; this is the solution for now as there is probably no more than one evaluated expression in each event sent to us     
   (iterate-events events-map "WAVELET_SELF_ADDED" (view-dev-this-blip))))

(defn-log do-replication-by-json [events-map]
  (unite-gadget-modifications (mapcat rep-op-to-operations (do-replication @rep-rules (incoming-map-to-rep-ops events-map)))))

(defn-log view-dev-and-do-replication [events-map]
 (wrap-json-operations-with-bundle (concat (view-dev events-map) (do-replication-by-json events-map))))

;(concat (view-dev events-map) (do-replication-by-json events-map)))




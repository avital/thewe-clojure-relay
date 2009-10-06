(ns we
 (:use compojure))

(defn update-db! [rep-ops]
  (swap! db into
    (for [rep-op rep-ops] [(:rep-loc rep-op) (:content rep-op)])))

(defroutes server
  (ANY "/wave"
    (let [rep-ops (incoming-map-to-rep-ops
                    (read-json
                      (params :events)))]
      (update-db! rep-ops)
      (json-str
        (rep-ops-to-outgoing-map @gadget-db
          (do-replication @rep-rules rep-ops))))))

(run-server {:port 31337}
  "/*" (servlet server))

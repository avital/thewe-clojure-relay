(ns we)

(def call-log (atom {}))

(defn macro? [expr]
  (not= (macroexpand-1 expr) expr))

(defmacro attempt [expr]
  `(try ~expr (catch Throwable t# t#)))

(defn log** [path result]
  (if (instance? Throwable result)
    (do
      (swap! call-log assoc-in path (str "Exception" result (.getStackTrace result)))
      (throw result))
    (swap! call-log assoc-in path (str result)))
  result)

(defmacro log* [path result]
  `(log** ~path (attempt ~result)))

(defmacro log [& path]
  (let [what (last path)]
    (if (seq? what)
      (cond
        (= (first what) 'do)
        `(do ~@(for [clause (rest what)] `(log ~clause)))

        (or (macro? what) (special-symbol? (first what)))
        `(log* '~path ~what)
        
        :else
        `(log* '~(concat path [:value])
           ~(for [clause what]
              `(log ~@path ~clause))))

      (cond
        (symbol? what)
        `(log* '~path ~what)
        
        :else what))))

(defmacro defn-log [name args & rest]
  `(defn ~name ~args (log (do ~@rest))))




; tests
(comment

(macroexpand-1 '(log (+ 2 2)))

(def x 2)
(def y 3)

(log (+ x y))

(defn-log f [x] (* (+ x x) x) 2)

(macroexpand-1 '(defn-log f [x] (+ x x) x))

(macroexpand-1 '(log (do x y)))

(log (do x y))

(f 2)

(log (+ 1 1))

(do
  (reset! call-log {})
  (f 2)
  (println (json-str @call-log)))

(do
  (reset! call-log {})
  (log (+ (/ 2 1) (/ 2 0)))
  (println (json-str @call-log)))

)
(ns we)

(def call-log (atom {}))

(defn macro? [expr]
  (not= (macroexpand-1 expr) expr))

(defmacro attempt [expr]
  `(try ~expr (catch Throwable t# t#)))

(def *log-path* [])

(def *log-counter* (atom 0))

(defn log** [result]
  (let [log-path (conj *log-path* :result)]
    (if (instance? Throwable result)
      (do
        (swap! call-log assoc-in log-path (str "Exception" result (.getStackTrace result)))
        (throw result))
      (swap! call-log assoc-in log-path (pr-str result)))
    result))

(defmacro log* [result]
  (println result)
  `(log** (attempt ~result)))

(let [x 1 y (inc x)] y)

(defn log-conj [pre new]
  (conj pre (str (swap! *log-counter* inc) "/" new)))

(defmacro log [what]
  (if (seq? what)
    (cond
      (#{'do} (first what))
      `(log* (~(first what) ~@(for [clause (rest what)] 
                                `(log ~clause))))
      
      (#{'if} (first what))
      `(binding [*log-path* (log-conj *log-path* '~what)]
         (log* (~(first what) ~@(for [clause (rest what)]
                                  `(log ~clause)))))

      (#{'let 'for} (first what))
      `(binding [*log-path* (log-conj *log-path* '~what)]
         (log* (~(first what) ~(vec (apply concat
                                 (for [[name val] (partition 2 (second what))]
                                   `(~name (log ~val))))) (log ~(nth what 2)))))

      (or (macro? what) (special-symbol? (first what)))
      `(binding [*log-path* (log-conj *log-path* '~what)]
         (log* ~what))

      :else
      `(binding [*log-path* (log-conj *log-path* '~what)]
         (log* ~(concat `(~(first what)) (for [clause (rest what)]
                  `(log ~clause))))))

    (cond
      (symbol? what)
      `(binding [*log-path* (log-conj *log-path* '~what)]
         (log* ~what))

      :else what)))

(defmacro defn-log [name args & rest]
  `(defn ~name ~args
     (binding [*log-path* (log-conj *log-path* (apply list (concat '(~name) ~args)))]
       (log (do ~@rest)))))




; tests
(comment

  (defn-log fact [n]
    (if (zero? n)
      1
      (* n (fact (dec n)))))

  (reset! call-log {})
  (reset! *log-counter* 0)
;  (macroexpand-1 '(log (for [x [[1 2] [3 4 5]] y x :when (even? y) z (range 1 y)] z)))

  (log (for [x [[1 2] [3 4 5]] y x :when (even? y) z (range 1 y)] z))
;  (log (* (inc 1) (inc 2)))
  (println (json-str @call-log))

  (log (if (zero? 2) (inc x) (inc y)))

  (log x)


(log (aveg 2 3))


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
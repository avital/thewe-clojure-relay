(ns we
  (:require clojure.contrib.pprint)
  (:use clojure.contrib.pprint))

(def *call-log* (atom {}))

(defn macro? [expr]
  (not= (macroexpand-1 expr) expr))

(defmacro attempt [expr]
  `(try ~expr (catch Throwable t# t#)))

(def *log-path* [])
(def *log-counter* (atom 0))
(def *unit-tests* (atom {}))
     
(defn log** [result]
  (let [log-path (conj *log-path* :result)]
    (if (instance? Throwable result)
      (do
        (swap! *call-log* assoc-in log-path (str "[Exception] " result))
        (throw result))
      (swap! *call-log* assoc-in log-path (with-out-str (pprint result))))
    result))

(defmacro log* [result]
  `(log** (attempt ~result)))

(defn log-conj [pre new]
  (conj pre (str (swap! *log-counter* inc) "/" new)))

(def *enable-logging* false)
(def *log-monads* false)

(defn third [l]
  (nth l 2))

(defn internal-log-form [expr]
  (let [func (first expr)
	standard-log-form `(~func ~@(for [clause (rest expr)]
				      `(log ~clause)))]
	(cond
	  (#{'do 'if 'and 'or} func)
	  standard-log-form
      
	  (#{'let 'for 'clojure.core/let 'clojure.core/for} func)
	  `(~func ~(second expr) (log ~(third expr))) 
      
	  (or (special-symbol? func)
	      (macro? expr))
	  expr
      
	  :else
	  standard-log-form)))

(defmacro log [expr]
  `(if *enable-logging*
     (binding [*log-path* (log-conj *log-path* '~expr)]
	~(if (seq? expr)
	   (let [func (first expr)]
	     (cond
	       (#{'iterate-events} func)
	       `(log ~(macroexpand-1 expr))
	    
	       :else
	       `(log* ~(internal-log-form expr))))
	
	   (cond
	     (symbol? expr)
	     `(log* ~expr)
	  
	     :else 
	     expr)))
     ~expr))

(defn clean-unit-tests! []
  (reset! *unit-tests* {}))

(defmacro defn-log [name args & rest]
  `(defn ~name ~args
     (let [result# 
	   (binding [*log-path* (log-conj *log-path* '(~name ~@args))]
	     (log* (do ~@(for [expr# rest] `(log ~expr#)))))]
       (if (empty? *log-path*) 
	 (let [expr# `(~'~name ~@~args)]
	   (swap! *unit-tests* assoc expr# result#)))
       result#)))

(defn run-tests []
  (for [[test expected] @*unit-tests* 
	:let [actual (eval test)] 
	:when (not= actual expected)]
    `(~test ~actual ~expected)))




; tests
  
  
  
(comment

  (defn-mark f [x y])

  (f 2 3)
  
  (macroexpand-1 '(defn-log f [x y] (+ x y)))

  (clean-unit-tests!)
  (defn-log f [x y] 2 (+ (inc x) (dec y)))
  (defn-log g [x y] (+ (f x y)
		       (f y x)))
  
  (g 2 3)
  (f 1 1)
  @*unit-tests*

  (eval '(f 2 3))
  
  (run-tests)

  (defn f [& args] `(f ~@args))

  (f 2 3)
  
  
  (def x 2)
  
  `(a (b ~x))

  '(b x)
  
  (defmacro iterate-events [x] `(+ ~x ~x))

  (macroexpand-1 '(iterate-events (inc 2)))

  (iterate-events 1)
  
  (log (iterate-events (inc 2)))

  (swap! call-log {})
  @call-log

  (println (json-str @call-log))
  
  (defn-log fact [n]
    (if (zero? n)
      1
      (* n (fact (dec n)))))

  (reset! call-log {})
  (reset! *log-counter* 0)
					;  (macroexpand-1 '(log (for [x [[1 2] [3 4 5]] y x :when (even? y) z (range 1 y)] z)))

  (log (for [x [1 2] :let [y (inc x)]] (+ x y)))
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
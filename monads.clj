(ns avital
  (:require clojure.contrib.monads)
  (:use clojure.contrib.monads)
  (:require clojure.contrib.accumulators)
  (:use clojure.contrib.accumulators))

; Original function form
(defn aveg [v] (/ (reduce + v) (count v)))

; Logged form with monads (manually written)
(defn aveg-with-logging [v]
  (domonad (writer-m empty-vector)
    [a1 [(reduce + v) [['(reduce + v) (reduce + v)]]]
     a2 [(/ a1 (count v)) [['(/ (reduce + v) (count v)) (/ a1 (count v))]]]]
    a2))

; Logged form with monads (manually written, no duplication of computation)
; Runs faster when computations done are relatively expensive
(defn aveg-with-logging-faster [v]
  (domonad (writer-m empty-vector)
    [:let [a1 (reduce + v)]
     a1-log [nil [['(reduce + v) a1]]]
     :let [a2 (/ a1 (count v))]
     a2-log [nil [['(/ (reduce + v) (count v)) a2]]]]
    a2))



; Help us do timing
(def *num-times* 100000)

(defmacro many-times [expr]
  `(time (dotimes [x# ~'*num-times*] ~expr)))




(many-times (aveg (range 10 1000)))
; "Elapsed time: 128.007295 msecs"

(many-times (aveg-with-logging (range 10 1000)))
; "Elapsed time: 10500.598395 msecs"

(many-times (aveg-with-logging-faster (range 10 1000)))
; "Elapsed time: 3508.199921 msecs"


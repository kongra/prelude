;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-03

(ns kongra.prelude-test
  (:require [clojure.test   :refer :all]
            [kongra.prelude :refer :all]))

;; (time (run-tests))

(def x (ref 3000)) ;; Dziecko 1
(def y (ref 5000)) ;; Matka
(def z (ref 2000)) ;; Dziecko 2

(dotimes [i 1000]
  (future
    ;; y := y - 1
    ;; x := x + 1
    (dosync
     (alter y - 1)
     (alter x + 1))))

(dotimes [i 250]
  (future
    ;; y := y - 4
    ;; z := z + 4
    (dosync
     (alter y - 4)
     (alter z + 4))))

;; (def val1 (future
;;             (Thread/sleep 10000)
;;             153900))

;; (println (deref val1))
;; (println @x @y @z)

(defn read-after
  []
  (println "--1" @x @y @z))

(read-after)

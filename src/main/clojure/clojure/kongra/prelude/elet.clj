;; Copyright (c) Konrad Grzanek
;; Created 2019-03-10
(ns clojure.kongra.prelude.elet
  (:require
   [clojure.kongra.ch
    :refer [defchP]]))

(set! *warn-on-reflection* true)

;; SIMPLIFIED EITHER-LIKE MONAD
(defrecord ^:private EletRight [value]
  clojure.lang.IDeref (deref [_] value))

(defrecord ^:private EletLeft [value]
  clojure.lang.IDeref (deref [_] value))

(prefer-method print-method java.util.Map clojure.lang.IDeref)

(defn right [value] (EletRight. value))
(defn left  [value] (EletLeft.  value))

(defn right? [x] (instance? EletRight  x))
(defn left?  [x] (instance? EletLeft   x))
(defn elet?  [x] (or (right? x) (left? x)))

(defchP chRight (right? x))
(defchP chLeft  (left?  x))
(defchP chElet  (elet?  x))

(defmacro elet
  [bindings & body]
  (assert (vector?      bindings))
  (assert (even? (count bindings)))

  (if-not (seq bindings)
    `(right (do ~@body))

    (let [[s expr & otherBindings] bindings]
      `(let [x# (chElet ~expr)]
         (if (left? x#)
           x#

           (let [~s @x#]
             (elet ~(vec otherBindings)
                   ~@body)))))))

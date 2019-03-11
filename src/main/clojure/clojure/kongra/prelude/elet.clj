;; Copyright (c) Konrad Grzanek
;; Created 2019-03-10
(ns clojure.kongra.prelude.elet
  (:require
   [clojure.kongra.ch
    :refer [defchP chOptional chAtom]]))

(set! *warn-on-reflection* true)

;; SIMPLIFIED EITHER-LIKE MONAD
(deftype ^:private EletRight [value]
  clojure.lang.IDeref (deref [_] value)

  Object
  (toString [_]
    (str "EletRight[{:value " (if (nil? value) "nil" value) "}]")))

(deftype ^:private EletLeft [err value]
  clojure.lang.IDeref (deref [_] err)

  Object
  (toString [_]
    (str "EletLeft[{:err " (if (nil?   err) "nil"   err)
         " :value "        (if (nil? value) "nil" value) "}]")))

(defn right [value]     (EletRight.     value))
(defn left  [err value] (EletLeft.  err value))

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

(defn leaving
  ([e]
   (leaving e nil))

  ([e errsAtom]
   (chElet
     (do (chElet e)
         (chOptional chAtom errsAtom)
         (if (right? e)
           e

           (do (when errsAtom
                 (swap! errsAtom conj (.err ^EletLeft e)))
               (right (.value ^EletLeft e))))))))

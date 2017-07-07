;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created     2015-12-19
;; Re-designed 2017-07-07

(ns kongra.prelude.locrefs
  (:refer-clojure :exclude [boolean byte short char int long float double
                            reset!]))

;; FAST MUTABLE, THREAD-UNSAFE (LOCAL) REFS FOR JAVA PRIMITIVE TYPES.
;; NO IDENTITY SEMANTICS.
;; (:require [clongra.locrefs :as lr])

;; CONSTRUCTORS

(defn boolean
  {:inline (fn [b] `(kongra.prelude.locrefs.LRboolean. ~b))}
  [b]
  (kongra.prelude.locrefs.LRboolean. (clojure.core/boolean b)))


(defn byte
  {:inline (fn [b] `(kongra.prelude.locrefs.LRbyte. ~b))}
  [b]
  (kongra.prelude.locrefs.LRbyte. (clojure.core/byte b)))


(defn short
  {:inline (fn [s] `(kongra.prelude.locrefs.LRshort. ~s))}
  [s]
  (kongra.prelude.locrefs.LRshort. (clojure.core/short s)))


(defn char
  {:inline (fn [c] `(kongra.prelude.locrefs.LRchar. ~c))}
  [c]
  (kongra.prelude.locrefs.LRchar. (clojure.core/char c)))


(defn int
  {:inline (fn [n] `(kongra.prelude.locrefs.LRint. ~n))}
  [n]
  (kongra.prelude.locrefs.LRint. (clojure.core/int n)))


(defn long
  {:inline (fn [n] `(kongra.prelude.locrefs.LRlong. ~n))}
  [n]
  (kongra.prelude.locrefs.LRlong. (clojure.core/long n)))


(defn float
  {:inline (fn [x] `(kongra.prelude.locrefs.LRfloat. ~x))}
  [x]
  (kongra.prelude.locrefs.LRfloat. (clojure.core/float x)))


(defn double
  {:inline (fn [x] `(kongra.prelude.locrefs.LRdouble. ~x))}
  [x]
  (kongra.prelude.locrefs.LRdouble. (clojure.core/double x)))


(defn obj
  {:inline (fn [x] `(kongra.prelude.locrefs.LRobject. ~x))}
  [x]
  (kongra.prelude.locrefs.LRobject. x))


;; ACCESSORS

(defmacro value  [lr]       `(.value ~lr))
(defmacro reset! [lr value] `(.set ~lr ~value))

;; TRANSFORMATION

(defmacro over!
  [lr form]
  (if (seq? form)
    (let [[f & args] form]
      `(let [lr# ~lr]
         (.set lr# (~f (.value lr#) ~@args)) lr#))

    `(let [lr# ~lr]
       (.set lr# (~form (.value lr#))) lr#)))

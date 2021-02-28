;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created     2015-12-19
;; Re-designed 2017-07-07
(ns clojure.kongra.prelude.locrefs
  (:refer-clojure
   :exclude [boolean byte short char int long float double
             reset!]))

(set! *warn-on-reflection* true)

;; FAST MUTABLE, THREAD-UNSAFE (LOCAL) REFS FOR JAVA PRIMITIVE TYPES.
;; NO IDENTITY SEMANTICS.
;; (:require [clongra.locrefs :as lr])

;; CONSTRUCTORS
(defn boolean
  {:inline (fn [b] `(jkongra.prelude.locrefs.LRboolean. ~b))}
  [b]
  (jkongra.prelude.locrefs.LRboolean. (clojure.core/boolean b)))

(defn byte
  {:inline (fn [b] `(jkongra.prelude.locrefs.LRbyte. ~b))}
  [b]
  (jkongra.prelude.locrefs.LRbyte. (clojure.core/byte b)))

(defn short
  {:inline (fn [s] `(jkongra.prelude.locrefs.LRshort. ~s))}
  [s]
  (jkongra.prelude.locrefs.LRshort. (clojure.core/short s)))

(defn char
  {:inline (fn [c] `(jkongra.prelude.locrefs.LRchar. ~c))}
  [c]
  (jkongra.prelude.locrefs.LRchar. (clojure.core/char c)))

(defn int
  {:inline (fn [n] `(jkongra.prelude.locrefs.LRint. ~n))}
  [n]
  (jkongra.prelude.locrefs.LRint. (clojure.core/int n)))

(defn long
  {:inline (fn [n] `(jkongra.prelude.locrefs.LRlong. ~n))}
  [n]
  (jkongra.prelude.locrefs.LRlong. (clojure.core/long n)))

(defn float
  {:inline (fn [x] `(jkongra.prelude.locrefs.LRfloat. ~x))}
  [x]
  (jkongra.prelude.locrefs.LRfloat. (clojure.core/float x)))

(defn double
  {:inline (fn [x] `(jkongra.prelude.locrefs.LRdouble. ~x))}
  [x]
  (jkongra.prelude.locrefs.LRdouble. (clojure.core/double x)))

(defn obj
  {:inline (fn [x] `(jkongra.prelude.locrefs.LRobject. ~x))}
  [x]
  (jkongra.prelude.locrefs.LRobject. x))

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

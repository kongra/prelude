;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26

(ns kongra.prelude
  (:require [primitive-math :as p]))

;; NON-NULL AND TYPE CHECKS FOR BOXED PRIMITIVES AND STRINGS

(defn ^String chString ;:- String -> String
  [s]
  (kongra.prelude.PrimitiveChecks/chString s))


(defn ^String chLong ;:- Long -> Long
  [l]
  (kongra.prelude.PrimitiveChecks/chLong l))


(defn ^String chDouble ;:- Double -> Double
  [d]
  (kongra.prelude.PrimitiveChecks/chDouble d))


(defn ^String chBoolean ;:- Boolean -> Boolean
  [b]
  (kongra.prelude.PrimitiveChecks/chBoolean b))


;; SYS/JVM

(defn room ;:- -> nil
  []
  (let [free-memory  (.. Runtime getRuntime freeMemory)
        total-memory (.. Runtime getRuntime totalMemory)
        max-memory   (.. Runtime getRuntime maxMemory)
        used-memory  (- total-memory free-memory)

        scale (fn [arg] (double (/ arg (* 1024 1024))))]

    (printf "Used  memory : %f MB\n" (scale used-memory))
    (printf "Free  memory : %f MB\n" (scale free-memory))
    (printf "Total memory : %f MB\n" (scale total-memory))
    (printf "Max   memory : %f MB\n" (scale max-memory))

    nil))


(defn gc
  ([] ;:- -> nil
   (gc true))

  ([room-after?] ;:- Boolean -> nil
   (System/gc)
   (when (chBoolean room-after?)
     (room)) nil))


(defmacro with-out-systemout ;:- & sexp -> Object|nil
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)] ~@body))


;; ABSTRACTION FOR ALL OBJECTS THAT MAY BE CONVERTED TO MILLISECONDS

(defprotocol Msecs
  (msecs [this])) ;:- Msecs this => this -> x|number?


;; ELAPSED TIME IN MILLIS

(deftype ^:private Stopwatch [^long start])

(defn stopwatch ;:- -> Stopwatch
  []
  (Stopwatch. (System/nanoTime)))


(defn elapsed-msecs ^double ;:- Stopwatch -> double
  [^Stopwatch s]
  (let [start (p/double (.start s))
        end   (p/double (System/nanoTime))]
    (p// (p/- end start) 1e6)))


(extend-protocol Msecs
  Stopwatch
  (msecs [this] (elapsed-msecs this)))


;; STRING ROUTINES

(defn blank? ;:- String|nil -> Boolean
  [s]
  (org.apache.commons.lang3.StringUtils/isBlank s))


(defn not-blank? ;:- String|nil -> Boolean
  [s]
  (not (blank? s)))


(defn ^String indent-string
  ([^long n] ;:- long -> String
   (indent-string n " "))

  ([^long n indent-with] ;:- long -> String -> String
   (let [indent-with (chString indent-with)
         sb (StringBuilder. (p/* n (.length indent-with)))]
     (dotimes [i n] (.append sb indent-with))
     (str sb))))


(defn ^String prefix-2-length ;:- long -> String -> String
  [^long n s]
  (let [s    (chString s)
	diff (p/- n (.length s))]
    (if (p/> diff 0) (str (indent-string diff) s) s)))


(defn ^String postfix-2-length ;:- long -> String -> String
  [^long n s]
  (let [s    (chString s)
	diff (p/- n (.length s))]
    (if (p/> diff 0) (str s (indent-string diff)) s)))


;; MISC. UTILS

(defn longs<
  "Generates an 'infinite' (upto Long.MAX_VALUE) series of increasing
  Long values."
  ([^long start] ;:- long -> (Long)
   (longs< start 1))

  ([^long start ^long step] ;:- long -> long -> (Long)
   (clojure.lang.LongRange/create start Long/MAX_VALUE step)))


(defn longs>
  "Generates an 'infinite' (downto Long.MIN_VALUE) series of decreasing
  Long values."
  ([^long start] ;:- long -> (Long)
   (longs> start -1))

  ([^long start ^long step] ;:- Long -> Long -> (Long)
   (clojure.lang.LongRange/create start Long/MIN_VALUE step)))


(defn N ;:- -> (Long)
  "A series of natural numbers."
  []
  (longs< 0))


(defn mark-last ;:- (a) -> (Boolean)
  "Takes a sequence (e0 e1 ... en) and returns (false false ... true)
  or (false false ...) if the argument is infinite. Lazy."
  [coll]
  (if-not (seq coll)
    '()
    (let [[_ & xs] coll]
      (if-not (seq xs)
        '(true)
        (cons false (lazy-seq (mark-last xs)))))))


(defn assoc-conj ;:- coll v => {k, coll v} -> k -> v -> {k, coll v}
  "Adds v to a collection that is a value for k in m. Uses empty-coll
  when no collection for k in m."
  [m k v empty-coll]
  (assoc m k (conj (get m k empty-coll) v)))


(defn vec-remove ;:- long -> [a] -> [a]
  "Returns a vector that is a result of removing n-th element from the
  vector v."
  [^long n v]
  (vec (concat (subvec v 0 n) (subvec v (p/inc n)))))

(defn make-longs ;:- long -> long[]
  {:inline (fn [size] `(kongra.prelude.Primitives/makeLongs ~size))}
  [^long size]
  (kongra.prelude.Primitives/makeLongs size))


(defn make-doubles ;:- long -> double[]
  {:inline (fn [size] `(kongra.prelude.Primitives/makeDoubles ~size))}
  [^long size]
  (kongra.prelude.Primitives/makeDoubles size))


(defn make-objects ;:- long -> Object[]
  {:inline (fn [size] `(kongra.prelude.Primitives/makeObjects ~size))}
  [^long size]
  (kongra.prelude.Primitives/makeObjects size))


(defn ref=
  "Alias of clojure.core/identical."
  {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))}
  [x y]
  (clojure.lang.Util/identical x y))


(defn bnot [b] ;:- Boolean -> Boolean
  {:inline (fn [b] `(kongra.prelude.Primitives/bnot ~b))}
  (kongra.prelude.Primitives/bnot b))


(defn not-nil? ;:- a|nil -> Boolean
  [x]
  (bnot (ref= x nil)))

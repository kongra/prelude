;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26

(ns kongra.prelude
  (:require [primitive-math :as       p]
            [kongra.ch      :refer :all]))

;; POSITIVE/NATURAL INTEGRALS

(defn pos-long? [^long n] (p/>  n 0))
(defn nat-long? [^long n] (p/>= n 0))

(defch chPoslong `(ch pos-long?))
(defch chNatlong `(ch pos-long?))

(defn pos-Long?
  [n]
  (and (chLong nil n) (pos-long? (.longValue ^Long n))))

(defn nat-Long?
  [n]
  (and (chLong nil n) (nat-long? (.longValue ^Long n))))

(defch chPosLong `(ch pos-Long?))
(defch chNatLong `(ch nat-Long?))

;; SYS/JVM

(defn room
  []
  (chUnit
   (let [free-memory  (.. Runtime getRuntime  freeMemory)
         total-memory (.. Runtime getRuntime totalMemory)
         max-memory   (.. Runtime getRuntime   maxMemory)
         used-memory  (- total-memory free-memory)

         scale (fn [arg] (double (/ arg (* 1024 1024))))]

     (printf "Used  memory : %f MB\n" (scale  used-memory))
     (printf "Free  memory : %f MB\n" (scale  free-memory))
     (printf "Total memory : %f MB\n" (scale total-memory))
     (printf "Max   memory : %f MB\n" (scale   max-memory)))))

(defn gc
  ([]
   (chUnit (gc true)))

  ([verbose?]
   (chUnit
    (do (System/gc)
        (when (chBoolean verbose?) (room))))))

(defmacro with-out-systemout
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)] ~@body))

;; ELAPSED TIME IN MILLIS

(deftype ^:private Stopwatch [^long start])

(defn ^Stopwatch stopwatch
  []
  (Stopwatch. (System/nanoTime)))

(defn msecs ^double
  [^Stopwatch s]
  (let [start (p/double (.start s))
        end   (p/double (System/nanoTime))]
    (p// (p/- end start) 1e6)))

;; STRING ROUTINES

(defn blank?
  [s]
  (chMaybe chString s)
  (chBoolean (org.apache.commons.lang3.StringUtils/isBlank s)))

(defn not-blank?
  [s]
  (chMaybe chString s)
  (chBoolean (not (blank? s))))

(defn indent-string
  ([^long n]
   (chString (indent-string n " ")))

  ([^long n ^String with]
   (chString (let [sb (StringBuilder. (p/* n (.length with)))]
               (dotimes [i n] (.append sb with))
               (str sb)))))

(defn prefix-2-length
  [^long n ^String s]
  (chString (let [diff (p/- n (.length s))]
              (if (p/> diff 0) (str (indent-string diff) s) s))))

(defn ^String postfix-2-length
  [^long n ^String s]
  (chString (let [diff (p/- n (.length s))]
              (if (p/> diff 0) (str s (indent-string diff)) s))))

;; MISC. UTILS

(defn longs<
  ([^long start]
   (chSeq (longs< start 1)))

  ([^long start ^long step]
   (chSeq (clojure.lang.LongRange/create start Long/MAX_VALUE step))))

(defn longs>
  ([^long start]
   (chSeq (longs> start -1)))

  ([^long start ^long step]
   (chSeq (clojure.lang.LongRange/create start Long/MIN_VALUE step))))

(defn N
  []
  (chSeq (longs< 0)))

(defn mark-last
  "Takes a sequence (e0 e1 ... en) and returns (false false ... true)
  or (false false ...) if the argument is infinite."
  [xs]
  (chSeq
   (if-not (seq (chSequential xs))
     '()
     (let [[_ & others] xs]
       (if-not (seq others)
         '(true)
         (cons false (lazy-seq (mark-last others))))))))

(defn assoc-conj
  "Adds v to a collection that is a value for k in m. Uses empty-coll
  when no collection for k in m."
  [m k v empty-coll]
  (chAssoc m) (chColl empty-coll)
  (chAssoc (assoc m k (conj (get m k empty-coll) v))))

(defn vec-remove
  "Returns a vector that is a result of removing n-th element from the
  vector v."
  [^long n v]
  (chVec v)
  (chVec (vec (concat (subvec v 0 n)
                      (subvec v (p/inc n))))))

(defn ref=
  "Alias of clojure.core/identical."
  {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))}
  [x y]
  (chBoolean (clojure.lang.Util/identical x y)))

(defn bnot [b]
  {:inline (fn [b] `(kongra.prelude.Primitives/bnot ~b))}
  (kongra.prelude.Primitives/bnot b))

(defn not-nil? ;:- a|nil -> Boolean
  [x]
  (bnot (ref= x nil)))

;; KIBIT CHEATERS

(defn lazy-cat' [s1 s2] (lazy-cat s1 s2))
(defn chSeq'    [x    ] (chSeq x))

;; ARRAYS AND RELATED CHECKS

(defn make-longs ^longs
  {:inline (fn [size] `(kongra.prelude.Primitives/makeLongs ~size))}
  [^long size]
  (kongra.prelude.Primitives/makeLongs size))

(defn make-doubles ^doubles
  {:inline (fn [size] `(kongra.prelude.Primitives/makeDoubles ~size))}
  [^long size]
  (kongra.prelude.Primitives/makeDoubles size))

(defn make-objects ^objects
  {:inline (fn [size] `(kongra.prelude.Primitives/makeObjects ~size))}
  [^long size]
  (kongra.prelude.Primitives/makeObjects size))

(defn longs?
  {:inline (fn [x] `(kongra.prelude.Primitives/isLongs ~x))}
  [x]
  (kongra.prelude.Primitives/isLongs x))

(defn doubles?
  {:inline (fn [x] `(kongra.prelude.Primitives/isDoubles ~x))}
  [x]
  (kongra.prelude.Primitives/isDoubles x))

(defn objects?
  {:inline (fn [x] `(kongra.prelude.Primitives/isObjects ~x))}
  [x]
  (kongra.prelude.Primitives/isObjects x))

(defch chLongs   `(ch   longs?))
(defch chDoubles `(ch doubles?))
(defch chObjects `(ch objects?))

;; KLEENE LOGIC

(deftype ^:private Kleene [s]
  Object
  (toString [_] (str s)))

(defchC chKleene Kleene)

(def ^Kleene KleeneTrue      (Kleene. "KleeneTrue"     ))
(def ^Kleene KleeneFalse     (Kleene. "KleeneFalse"    ))
(def ^Kleene KleeneUndefined (Kleene. "KleeneUndefined"))

(defn Kleene-not ;:- Kleene -> Kleene
  [x]
  (chKleene
   (let [x (chKleene x)]
     (cond (ref= KleeneTrue  x) KleeneFalse
           (ref= KleeneFalse x) KleeneTrue
           :else                KleeneUndefined))))

(defmacro Kleene-and
  ([]  `(chKleene KleeneTrue))
  ([x] `(chKleene ~x))
  ([x & xs]
   `(chKleene
     (let [x# (chKleene ~x)]
       (cond (ref= KleeneFalse x#) KleeneFalse
             (ref= KleeneTrue  x#) (Kleene-and ~@xs)

             :else ;; KleeneUndefined
             (if (ref= KleeneFalse (Kleene-and ~@xs))
               KleeneFalse
               KleeneUndefined))))))

(defmacro Kleene-or
  ([]  `(chKleene KleeneFalse))
  ([x] `(chKleene ~x))
  ([x & xs]
   `(chKleene
     (let [x# (chKleene ~x)]
       (cond (ref= KleeneTrue  x#) KleeneTrue
             (ref= KleeneFalse x#) (Kleene-or ~@xs)

             :else ;; KleeneUndefined
             (if (ref= KleeneTrue (Kleene-or ~@xs))
               KleeneTrue
               KleeneUndefined))))))

;; PSEUDORANDOM NUMBERS GENERATORS

(defchC chRandom        java.util.Random) (regch chRandom)
(defchC chRandist kongra.prelude.Randist)

(defn uuid!
  []
  (chString (.. java.util.UUID randomUUID toString)))

(defn make-MersenneTwister
  [^long seed]
  (chRandom (let [bs (byte-array 16)]
              (kongra.prelude.Bits/putLong bs 0 seed)
              (org.uncommons.maths.random.MersenneTwisterRNG. bs))))

(def ^:private randist-state
  (atom (kongra.prelude.Randist. (make-MersenneTwister 0))))

(defn ^kongra.prelude.Randist randist
  []
  (chRandist (deref (chAtom randist-state))))

(defn set-seed!
  [^long seed]
  (chUnit
   (do (reset! (chAtom randist-state)
               (kongra.prelude.Randist.(chRandom (make-MersenneTwister seed))))
       nil)))

(defmacro randgen!
  [method & args]
  `(~method (randist) ~@args))

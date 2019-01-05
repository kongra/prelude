;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26
(ns clojure.kongra.prelude
  (:require
   [primitive-math
    :as p]

   [clojure.math.numeric-tower
    :as m]

   [clojure.kongra.ch
    :refer [defchP chBool chUnit chMaybe chString chAtom
            chNumber chNatLong chIfn
            chSeq chSequential chAssoc chColl chVector]]))

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
        (when (chBool verbose?) (room))))))

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
  (chBool (org.apache.commons.lang3.StringUtils/isBlank s)))

(defn not-blank?
  [s]
  (chMaybe chString s)
  (chBool (not (blank? s))))

#_(defn strip
  [s]
  (chMaybe chString s)
  (chMaybe chString (org.apache.commons.lang3.StringUtils/strip s)))

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
  "Takes (e0 e1 ... en) and returns (false false ... true) or (false false ...)
  if the argument is infinite."
  [xs]
  (chSequential xs)
  (chSeq
   (if-not (seq xs)
     '()
     (let [[_ & others] xs]
       (if-not (seq others)
         '(true)
         (cons false (lazy-seq (mark-last others))))))))

(defn assoc-conj
  "Adds v to a collection that is a value for k in m. Uses empty-coll
  when no collection for k in m."
  [m k v empty-coll]
  (chAssoc         m)
  (chColl empty-coll)
  (chAssoc (assoc m k (conj (chColl (get m k empty-coll)) v))))

(defn vec-remove
  "Returns a vector that is a result of removing n-th element from the
  vector v."
  [^long n v]
  (chVector v)
  (chVector (vec (concat (subvec v 0 n)
                      (subvec v (p/inc n))))))

(defn ref=
  "Alias of clojure.core/identical."
  {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))}
  [x y]
  (chBool (clojure.lang.Util/identical x y)))

(defn bnot [b]
  {:inline (fn [b] `(jkongra.prelude.Primitives/bnot ~b))}
  (jkongra.prelude.Primitives/bnot b))

(defmacro synchronized {:style/indent 1}
  [monitor & body]
  `(jkongra.prelude.Synchronized/invoke
    (chSome ~monitor)
    (fn [] ~@body)))

;; KIBIT CHEATERS
(defn lazy-cat' [s1 s2] (lazy-cat s1 s2))
(defn chSeq'    [x    ] (chSeq x))

;; ARRAYS AND RELATED CHECKS
(defn make-longs ^longs
  {:inline (fn [size] `(jkongra.prelude.Primitives/makeLongs ~size))}
  [^long size]
  (jkongra.prelude.Primitives/makeLongs size))

(defn make-doubles ^doubles
  {:inline (fn [size] `(jkongra.prelude.Primitives/makeDoubles ~size))}
  [^long size]
  (jkongra.prelude.Primitives/makeDoubles size))

(defn make-objects ^objects
  {:inline (fn [size] `(jkongra.prelude.Primitives/makeObjects ~size))}
  [^long size]
  (jkongra.prelude.Primitives/makeObjects size))

(defn longs?
  {:inline (fn [x] `(jkongra.prelude.Primitives/isLongs ~x))}
  [x]
  (jkongra.prelude.Primitives/isLongs x))

(defn doubles?
  {:inline (fn [x] `(jkongra.prelude.Primitives/isDoubles ~x))}
  [x]
  (jkongra.prelude.Primitives/isDoubles x))

(defn objects?
  {:inline (fn [x] `(jkongra.prelude.Primitives/isObjects ~x))}
  [x]
  (jkongra.prelude.Primitives/isObjects x))

(defchP chLongs   (longs?   x))
(defchP chDoubles (doubles? x))
(defchP chObjects (objects? x))

;; KLEENE LOGIC
(deftype ^:private Kleene [s]
  Object
  (toString [_] (str s)))

(defchP chKleene (instance? Kleene x))

(def ^Kleene KleeneTrue      (Kleene. "KleeneTrue"     ))
(def ^Kleene KleeneFalse     (Kleene. "KleeneFalse"    ))
(def ^Kleene KleeneUndefined (Kleene. "KleeneUndefined"))

(defn Kleene-not ;:- Kleene -> Kleene
  [x]
  (chKleene x)
  (chKleene
   (cond (ref= KleeneTrue  x) KleeneFalse
         (ref= KleeneFalse x) KleeneTrue
         :else                KleeneUndefined)))

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
(defchP chRandom  (instance? java.util.Random        x))
(defchP chRandist (instance? jkongra.prelude.Randist x))

(defn uuid!
  []
  (chString (.. java.util.UUID randomUUID toString)))

(defn make-MersenneTwister
  [^long seed]
  (chRandom
   (let [bs (byte-array 16)]
     (jkongra.prelude.Bits/putLong bs 0 seed)
     (org.uncommons.maths.random.MersenneTwisterRNG. bs))))

(def ^:private randist-state
  (atom (jkongra.prelude.Randist. (make-MersenneTwister 0))))

(defn ^jkongra.prelude.Randist randist
  []
  (chRandist (deref (chAtom randist-state))))

(defn set-seed!
  [^long seed]
  (chUnit
   (do (reset! (chAtom randist-state)
               (jkongra.prelude.Randist. (chRandom (make-MersenneTwister seed))))
       nil)))

(defmacro randgen!
  [method & args]
  `(~method (randist) ~@args))


;; BASIC MATH
(defn **
  "Convenience wrapper around clojure.contrib.ccmath/expt."
  [base pow]
  (chNumber
      (do
        (chNumber base)
        (chNumber  pow)
        (m/expt base pow))))

(defn **-N
  "x to the power of n such that n is a Natural long"
  ([x ^long n] (**-N *' x n))

  ([multop x ^long n]
   (chNumber   x)
   (chNatLong  n)
   (chIfn multop)
   (chNumber
    (loop [x x n n result (Long/valueOf 1)]
      (cond (p/zero? n)
            result

            (jkongra.prelude.Maths/isEven n)
            (recur (multop x x) (p// n 2) result)

            :else
            (recur x (p/dec n) (multop x result)))))))

;; LOCREFS CHECKS
;; Defined here and not in the original kongra.prelude.locrefs for convenience
;; of use when requiring :all kongra.prelude
(defchP chLRboolean (instance? jkongra.prelude.locrefs.LRboolean x))
(defchP chLRbyte    (instance? jkongra.prelude.locrefs.LRbyte    x))
(defchP chLRshort   (instance? jkongra.prelude.locrefs.LRshort   x))
(defchP chLRchar    (instance? jkongra.prelude.locrefs.LRchar    x))
(defchP chLRint     (instance? jkongra.prelude.locrefs.LRint     x))
(defchP chLRlong    (instance? jkongra.prelude.locrefs.LRlong    x))
(defchP chLRfloat   (instance? jkongra.prelude.locrefs.LRfloat   x))
(defchP chLRdouble  (instance? jkongra.prelude.locrefs.LRdouble  x))
(defchP chLRobject  (instance? jkongra.prelude.locrefs.LRobject  x))

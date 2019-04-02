;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26
(ns clojure.kongra.prelude
  (:require
   [primitive-math
    :as p]

   [clojure.math.numeric-tower
    :as m]

   [cljc.kongra.ch
    :refer [defchP chBool chUnit chString chAtom
            chNumber chNatLong chIfn
            chSeq chSequential chAssoc chColl chVector]]))

(set! *warn-on-reflection* true)

;; SYS/JVM
(defn room
  []
  (chUnit
   (let [memFree  (.. Runtime getRuntime  freeMemory)
         memTotal (.. Runtime getRuntime totalMemory)
         memMax   (.. Runtime getRuntime   maxMemory)
         memUsed  (-  memTotal memFree)

         scale    (fn [arg]
                    (double (/ arg (* 1024 1024))))]

     (printf "Used  memory : %f MB\n" (scale  memUsed))
     (printf "Free  memory : %f MB\n" (scale  memFree))
     (printf "Total memory : %f MB\n" (scale memTotal))
     (printf "Max   memory : %f MB\n" (scale   memMax)))))

(defn gc
  ([]
   (chUnit (gc true)))

  ([verbose?]
   (chUnit
    (do (System/gc)
        (when (chBool verbose?) (room))))))

(defmacro withOutSystemout
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)] ~@body))

;; ELAPSED TIME IN MILLIS

(deftype ^:private Stopwatch [^long start])

(defn stopwatch ^Stopwatch
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
  (org.apache.commons.lang3.StringUtils/isBlank s))

(defn nonBlank?
  [s]
  (not (blank? s)))

(defn indentString
  ([^long n]
   (chString
     (indentString n " ")))

  ([^long n ^String with]
   (chString
     (let [sb (StringBuilder. (p/* n (.length with)))]
       (dotimes [i n] (.append sb with))
       (str sb)))))

(defn prefix2length
  [^long n ^String s]
  (chString
    (let [diff (p/- n (.length s))]
      (if (p/> diff 0) (str (indentString diff) s) s))))

(defn postfix2length
  [^long n ^String s]
  (chString
    (let [diff (p/- n (.length s))]
      (if (p/> diff 0) (str s (indentString diff)) s))))

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

(defn markLast
  "Takes (e0 e1 ... en) and returns (false false ... true) or (false false ...)
  if the argument is infinite."
  [xs]
  (chSeq
    (do (chSequential xs)
        (if-not (seq xs)
          '()
          (let [[_ & others] xs]
            (if-not (seq others)
              '(true)
              (cons false (lazy-seq (markLast others)))))))))

(defn assoConj
  "Adds v to a collection that is a value for k in m. Uses emptyColl
  when no collection for k in m."
  [m k v emptyColl]
  (chAssoc
    (do (chAssoc        m)
        (chColl emptyColl)
        (assoc m k (conj (chColl (get m k emptyColl)) v)))))

(defn vecRemove
  "Returns a vector that is a result of removing n-th element from the
  vector v."
  [^long n v]
  (chVector
    (do (chVector v)
        (vec (concat (subvec v 0 n)
                     (subvec v (p/inc n)))))))

(defn ref=
  "Alias of clojure.core/identical."
  {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))}
  [x y]
  (clojure.lang.Util/identical x y))

(defn bnot [b]
  {:inline (fn [b] `(jkongra.prelude.Primitives/bnot ~b))}
  (jkongra.prelude.Primitives/bnot b))

(defmacro synchronized {:style/indent 1}
  [monitor & body]
  `(jkongra.prelude.Synchronized/invoke
    (chSome ~monitor)
    (fn [] ~@body)))

;; KIBIT CHEATERS
(defn lazyCat [s1 s2] (lazy-cat s1 s2))
(defn chSeq'  [x    ] (chSeq x))

;; ARRAYS AND RELATED CHECKS
(defn makeLongs ^longs
  {:inline (fn [size] `(jkongra.prelude.Primitives/makeLongs ~size))}
  [^long size]
  (jkongra.prelude.Primitives/makeLongs size))

(defn makeDoubles ^doubles
  {:inline (fn [size] `(jkongra.prelude.Primitives/makeDoubles ~size))}
  [^long size]
  (jkongra.prelude.Primitives/makeDoubles size))

(defn makeObjects ^objects
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
  (chKleene
    (do (chKleene x)
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
(defchP chRandom  (instance? java.util.Random        x))
(defchP chRandist (instance? jkongra.prelude.Randist x))

(defn uuid!
  []
  (chString (.. java.util.UUID randomUUID toString)))

(defn makeMersenneTwister
  [^long seed]
  (chRandom
   (let [bs (byte-array 16)]
     (jkongra.prelude.Bits/putLong bs 0 seed)
     (org.uncommons.maths.random.MersenneTwisterRNG. bs))))

(def ^:private randistState
  (atom (jkongra.prelude.Randist. (makeMersenneTwister 0))))

(defn ^jkongra.prelude.Randist randist
  []
  (chRandist (deref (chAtom randistState))))

(defn setSeed!
  [^long seed]
  (chUnit
   (do (reset! (chAtom randistState)
               (jkongra.prelude.Randist. (chRandom (makeMersenneTwister seed))))
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
   (chNumber
    (do (chNumber   x)
        (chNatLong  n)
        (chIfn multop)
        (loop [x x
               n n
               result (Long/valueOf 1)]
          (cond (p/zero? n)
                result

                (jkongra.prelude.Maths/isEven n)
                (recur (multop x x) (p// n 2) result)

                :else
                (recur x (p/dec n) (multop x result))))))))

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

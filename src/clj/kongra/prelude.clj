;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26

(ns kongra.prelude
  (:require [primitive-math :as    p]
            [clojure.set    :as cset]))

;; CH - INLINED ASSERTIONS AND MEMBERSHIP PREDICATES FOR ALGEBRAIC DATA-TYPES

(defn chmsg
  [x]
  (if (nil? x) "Illegal nil value"
      (str "Illegal value " x " of type " (.getName (class x)))))

;; NAME+PRED → CH
(defmacro ch {:style/indent 1}
  ([pred   x]
   (let [x'   (gensym "x__")
         expr (seq (conj (vec pred) x'))]
     `(let [~x' ~x] (assert ~expr (chmsg ~x')) ~x')))

  ([pred _ x] ; as-pred
   (let [expr (seq (conj (vec pred) x))]
     `(boolean ~expr))))

(defmacro defch {:style/indent 1}
  ([chname     pred] `(defch ~chname x# ~pred))
  ([chname arg pred]
   `(defn ~chname
      ([~arg    ] (ch ~pred                ~arg))
      ([~'_ ~arg] (ch ~pred #_ as-pred nil ~arg)))))

;; CLASS → CH
(defmacro chc {:style/indent 1}
  ([c   x] `(ch (instance? ~c)                ~x))
  ([c _ x] `(ch (instance? ~c) #_ as-pred nil ~x)))

(defmacro defchc {:style/indent 1}
  [chname c]
  `(defch ~chname (chc ~c #_ as-pred nil)))

(defmacro chLike {:style/indent 1}
  ([y   x] `(chc (class ~y)                ~x))
  ([y _ x] `(chc (class ~y) #_ as-pred nil ~x)))

;; UNIT (NIL)
(defch chUnit (nil?))

;; NON-UNIT (NOT NIL)
(declare      not-nil?)
(defch chObj (not-nil?))

;; CO-PRODUCT (DISCRIMINATED UNION)
(defmacro ch| {:style/indent 1}
  ([chs x]
   (assert (vector? chs))
   (assert (seq     chs))
   (let [x'       (gensym "x__")
         pred-chs (map (fn [ch] `(~ch #_ as-pred nil ~x')) (butlast chs))
         ch       (list (last chs) x')
         n        (count pred-chs)]

     (cond (zero? 0) `(let [~x' ~x] ~ch)
           (= n   1) `(let [~x' ~x] (when-not ~(first pred-chs) ~ch) ~x')
           :else     `(let [~x' ~x] (when-not (or ~@pred-chs)   ~ch) ~x'))))

  ([chs _ x]
   (assert (vector? chs))
   (assert (seq     chs))
   (let [x'       (gensym "x")
         pred-chs (map (fn [ch] `(~ch #_ as-pred nil ~x')) chs)
         n        (count pred-chs)]

     (if (= n 1)
       `(let [~x' ~x] ~(first pred-chs))
       `(let [~x' ~x] (or ~@pred-chs))))))
;; aliasing like: (defch chABC (ch| [chA chB chC] #_ as-pred nil)

;; MAYBE
(defmacro chMaybe {:style/indent 1}
  ([ch   x] `(ch| [chUnit ~ch]                ~x))
  ([ch _ x] `(ch| [chUnit ~ch] #_ as-pred nil ~x)))
;; aliasing like: (defch chMaybeA (chMaybe chA #_ as-pred nil)

;; EITHER
(defmacro chEither {:style/indent 1}
  ([chl chr   x] `(ch| [~chl ~chr]                ~x))
  ([chl chr _ x] `(ch| [~chl ~chr] #_ as-pred nil ~x)))
;; aliasing like: (defch chEitherAB (chEither chA chB #_ as-pred nil)

;; CHS REGISTRY
(def ^:private CHS (atom {}))

(defn regch*
  [chname ch]
  (chUnit
   (do
     (assert (string? chname))
     (assert (fn?         ch))
     (swap! CHS
            (fn [m]
              (when (m chname)
                (println "WARNING: chname already in use:" chname))
              (assoc m chname ch))) nil)))

(defmacro regch
  [ch]
  (assert (symbol? ch))
  `(regch* ~(str ch) ~ch))

(declare chSet)

(defn chs
  ([x]
   (chSet (->> @CHS
               (filter (fn [[_ pred]] (pred #_as-pred nil x)))
               (map first)
               (apply sorted-set))))
  ([x & xs]
    (chSet (->> (cons x xs) (map chs) (apply cset/intersection)))))

(defn chdiffs
  [& xs]
  (chSet (->> xs (map chs) (apply cset/difference))))

;; COMMON CHS
(defchc chAgent           clojure.lang.Agent) (regch      chAgent)
(defchc chAtom             clojure.lang.Atom) (regch       chAtom)
(defchc chASeq             clojure.lang.ASeq) (regch       chASeq)
(defchc chBoolean                    Boolean) (regch    chBoolean)
(defchc chDeref          clojure.lang.IDeref) (regch      chDeref)
(defchc chDouble                      Double) (regch     chDouble)
(defchc chIndexed       clojure.lang.Indexed) (regch    chIndexed)
(defchc chLazy          clojure.lang.LazySeq) (regch       chLazy)
(defchc chLong                          Long) (regch       chLong)
(defchc chLookup        clojure.lang.ILookup) (regch     chLookup)
(defchc chRef               clojure.lang.Ref) (regch        chRef)
(defchc chSeqable       clojure.lang.Seqable) (regch    chSeqable)
(defchc chSequential clojure.lang.Sequential) (regch chSequential)

(defch  chAssoc               (associative?)) (regch      chAssoc)
(defch  chChar                       (char?)) (regch       chChar)
(defch  chClass                     (class?)) (regch      chClass)
(defch  chColl                       (coll?)) (regch       chColl)
(defch  chCounted                 (counted?)) (regch    chCounted)
(defch  chDecimal                 (decimal?)) (regch    chDecimal)
(defch  chDelay                     (delay?)) (regch      chDelay)
(defch  chFloat                     (float?)) (regch      chFloat)
(defch  chFn                           (fn?)) (regch         chFn)
(defch  chFuture                   (future?)) (regch     chFuture)
(defch  chIfn                         (ifn?)) (regch        chIfn)
(defch  chInteger                 (integer?)) (regch    chInteger)
(defch  chKeyword                 (keyword?)) (regch    chKeyword)
(defch  chList                       (list?)) (regch       chList)
(defch  chMap                         (map?)) (regch        chMap)
(defch  chNumber                   (number?)) (regch     chNumber)
(defch  chRatio                     (ratio?)) (regch      chRatio)
(defch  chRational               (rational?)) (regch   chRational)
(defch  chRecord                   (record?)) (regch     chRecord)
(defch  chReduced                 (reduced?)) (regch    chReduced)
(defch  chReversible           (reversible?)) (regch chReversible)
(defch  chSeq                         (seq?)) (regch        chSeq)
(defch  chSet                         (set?)) (regch        chSet)
(defch  chSorted                   (sorted?)) (regch     chSorted)
(defch  chString                   (string?)) (regch     chString)
(defch  chSymbol                   (symbol?)) (regch     chSymbol)
(defch  chVar                         (var?)) (regch        chVar)
(defch  chVec                      (vector?)) (regch        chVec)

(defchc chJavaColl      java.util.Collection) (regch   chJavaColl)
(defchc chJavaList            java.util.List) (regch   chJavaList)
(defchc chJavaMap              java.util.Map) (regch    chJavaMap)
(defchc chJavaSet              java.util.Set) (regch    chJavaSet)

;; INTEGRAL CHS/CONSTRS
(deftype           PosLong [^long uncons])
(defchc          chPosLong PosLong)
(defn ^PosLong consPosLong [^long n] (assert (p/> n 0)) (PosLong. n))

(deftype           NatLong [^long uncons])
(defchc          chNatLong NatLong)
(defn ^NatLong consNatLong [^long n] (assert (p/>= n 0)) (NatLong. n))

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

(defn ref=
  "Alias of clojure.core/identical."
  {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))}
  [x y]
  (chBoolean (clojure.lang.Util/identical x y)))

(defn bnot [b]
  {:inline (fn [b] `(chBoolean (kongra.prelude.Primitives/bnot ~b)))}
  (chBoolean (kongra.prelude.Primitives/bnot b)))

(defn not-nil? ;:- a|nil -> Boolean
  [x]
  (chBoolean (bnot (ref= x nil))))

;; KLEENE LOGIC

(deftype ^:private Kleene [s]
  Object
  (toString [_] (str s)))

(defchc chKleene Kleene)

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

;; TREE SEARCH ROUTINES FROM BY PAIP , CHAPTER 6.4
;; FOR MORE SEE clongra.search

(defn breadth-first-combiner
  [nodes new-nodes]
  (chSeq (lazy-cat (chSeq nodes) (chSeq new-nodes))))

(defn depth-first-combiner
  [nodes new-nodes]
  (chSeq (lazy-cat (chSeq new-nodes) (chSeq nodes))))

;; (defn tree-search
;;   [start goal? adjs combiner]
;;   (loop [nodes (list start)]
;;     (when (seq (chSeq nodes)))))

;; (defn tree-search
;;   ;;:- (a) -> (a -> Boolean) -> (a -> (a)) -> ((a) -> (a) -> (a)) -> a|nil
;;   "Searches state-spaces that have the form of trees. Starts with
;;   a sequence of states and performs the search according to the
;;   goal? predicate, generator of nodes adjacent do a given node
;;   and combiner responsible of adding nodes to the search
;;   collection of nodes."
;;   [states goal? adjacent combiner]
;;   (when (seq states)
;;     (let [obj (first states)]
;;       (if (goal? obj)
;;         obj

;;         (recur (combiner (adjacent obj) (rest states))
;;                goal?
;;                adjacent
;;                combiner)))))

;; (defn breadth-first-tree-levels
;;   ;;:- a -> (a -> (a)) -> ((a))
;;   "Returns a lazy collection of lazy sequences of nodes belonging
;;   to subsequent tree levels."
;;   [start adjacent]
;;   (->> (list start)
;;        (iterate #(mapcat adjacent %))
;;        (take-while seq)))

;; (defn breadth-first-tree-seq
;;   "Returns a lazy sequence of tree nodes starting with the passed
;;   start node where adjacent is a function generating nodes
;;   adjacent to it's argument.

;;   Goes on infinitely unless the limiting depth specified."
;;   ([start adjacent] ;:- a -> (a -> (a)) -> (a)
;;      (apply concat (breadth-first-tree-levels start adjacent)))

;;   ([start adjacent depth] ;:- a -> (a -> (a)) -> Positive Long -> (a)
;;      (->> (breadth-first-tree-levels start adjacent)
;;           (take depth)
;;           (reduce concat))))

;; RANDOM UTILS

(defchc chRandom        java.util.Random) (regch chRandom)
(defchc chRandist kongra.prelude.Randist)

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

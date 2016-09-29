;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26

(ns kongra.prelude
  (:require [primitive-math :as    p]
            [clojure.set    :as cset]))

;; TYPE CHECKS

(defmacro defch
  ([chname pred]
   `(defch ~chname x# ~pred))

  ([chname arg pred]
   `(do (defn ~chname
          [~arg]
          (assert ~(concat pred (list arg)) ) ~arg)

        (defchinfo ~chname ~arg ~pred))))

(def ^:private chinfos-state (atom {}))

(defn add-chinfo! ; To be used in (defch ...)
  [chname pred]
  (swap! chinfos-state
         (fn [m]
           (when (m chname)
             (println "WARNING: chname already in use: " chname))
           (assoc m chname pred))))

(defmacro defchinfo ; To be used in (defch ...)
  ([chname arg pred]
   `(add-chinfo! (str '~chname) (fn [~arg] ~(concat pred (list arg))))))

(defch chASeq       (instance?       clojure.lang.ASeq))
(defch chBoolean    (instance?                 Boolean))
(defch chDouble     (instance?                  Double))
(defch chIndexed    (instance?    clojure.lang.Indexed))
(defch chLazy       (instance?    clojure.lang.LazySeq))
(defch chLong       (instance?                    Long))
(defch chLookup     (instance?    clojure.lang.ILookup))
(defch chSeqable    (instance?    clojure.lang.Seqable))
(defch chSequential (instance? clojure.lang.Sequential))

(defch chAssoc     (associative?))
(defch chChar             (char?))
(defch chClass           (class?))
(defch chColl             (coll?))
(defch chCounted       (counted?))
(defch chDecimal       (decimal?))
(defch chDelay           (delay?))
(defch chFloat           (float?))
(defch chFn                 (fn?))
(defch chFuture         (future?))
(defch chIfn               (ifn?))
(defch chInteger       (integer?))
(defch chKeyword       (keyword?))
(defch chList             (list?))
(defch chMap               (map?))
(defch chNumber         (number?))
(defch chRatio           (ratio?))
(defch chRational     (rational?))
(defch chRecord         (record?))
(defch chReduced       (reduced?))
(defch chReversible (reversible?))
(defch chSeq               (seq?))
(defch chSet               (set?))
(defch chSorted         (sorted?))
(defch chString         (string?))
(defch chSymbol         (symbol?))
(defch chVar               (var?))
(defch chVec            (vector?))

(defn chinfos
  [x]
  (chSet (->> @chinfos-state
              (filter (fn [[_ pred]] (pred x)))
              (map first)
              set)))

(defn chcommons
  [& xs]
  (chSet (apply cset/intersection (map chinfos xs))))

(defn chdiffs
  [& xs]
  (chSet (apply cset/difference (map chinfos xs))))

;; SYS/JVM

(defn room
  []
  (let [free-memory  (.. Runtime getRuntime freeMemory)
        total-memory (.. Runtime getRuntime totalMemory)
        max-memory   (.. Runtime getRuntime maxMemory)
        used-memory  (- total-memory free-memory)

        scale (fn [arg] (double (/ arg (* 1024 1024))))]

    (printf "Used  memory : %f MB\n" (scale used-memory))
    (printf "Free  memory : %f MB\n" (scale free-memory))
    (printf "Total memory : %f MB\n" (scale total-memory))
    (printf "Max   memory : %f MB\n" (scale max-memory))))

(defn gc
  ([]
   (gc true))

  ([verbose?]
   (System/gc)
   (when (chBoolean verbose?) (room))))

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
  (chBoolean (org.apache.commons.lang3.StringUtils/isBlank s)))

(defn not-blank?
  [s]
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
  (chAssoc (assoc (chAssoc m) k (conj (get m k (chColl empty-coll)) v))))

(defn vec-remove ;:- long -> [a] -> [a]
  "Returns a vector that is a result of removing n-th element from the
  vector v."
  [^long n v]
  (chVec (vec (concat (subvec (chVec v) 0      n)
                      (subvec        v  (p/inc n))))))

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
  {:inline (fn [x y] `(chBoolean (. clojure.lang.Util identical ~x ~y)))}
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

(defch chKleene (instance? Kleene))

(def ^Kleene KleeneTrue      (Kleene. "KleeneTrue"     ))
(def ^Kleene KleeneFalse     (Kleene. "KleeneFalse"    ))
(def ^Kleene KleeneUndefined (Kleene. "KleeneUndefined"))

(defn Kleene-not ;:- Kleene -> Kleene
  [x]
  (let [x (chKleene x)]
    (cond (ref= KleeneTrue  x) KleeneFalse
          (ref= KleeneFalse x) KleeneTrue
          :else                KleeneUndefined)))

(defmacro Kleene-and
  ([] KleeneTrue)
  ([x] `(chKleene ~x))
  ([x & xs]
   `(let [x# (chKleene ~x)]
      (cond (ref= KleeneFalse x#) KleeneFalse
            (ref= KleeneTrue  x#) (Kleene-and ~@xs)
            :else ;; KleeneUndefined
            (if (ref= KleeneFalse (Kleene-and ~@xs))
              KleeneFalse
              KleeneUndefined)))))

(defmacro Kleene-or
  ([]  KleeneFalse)
  ([x] `(chKleene ~x))
  ([x & xs]
   `(let [x# (chKleene ~x)]
      (cond (ref= KleeneTrue  x#) KleeneTrue
            (ref= KleeneFalse x#) (Kleene-or ~@xs)
            :else ;; KleeneUndefined
            (if (ref= KleeneTrue (Kleene-or ~@xs))
              KleeneTrue
              KleeneUndefined)))))

;; ;; TREE SEARCH ROUTINES FROM BY PAIP , CHAPTER 6.4
;; ;; FOR MORE SEE clongra.search

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

;; (defn depth-first-combiner ;:- (a) -> (a) -> (a)
;;   "The combiner for the depth-first-search."
;;   [new-nodes states]
;;   (lazy-cat new-nodes states))

;; (defn breadth-first-combiner ;:- (a) -> (a) -> (a)
;;   "The combiner for the breadth-first-search."
;;   [new-nodes states]
;;   (lazy-cat states new-nodes))

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

(defn uuid!
  []
  (chString (.. java.util.UUID randomUUID toString)))

(defch chRandom  (instance?       java.util.Random))
(defch chRandist (instance? kongra.prelude.Randist))

(defn make-MersenneTwister
  [^long seed]
  (chRandom (let [bs (byte-array 16)]
              (kongra.prelude.Bits/putLong bs 0 seed)
              (org.uncommons.maths.random.MersenneTwisterRNG. bs))))

(def ^:private randist-state
  (atom (kongra.prelude.Randist. (make-MersenneTwister 0))))

(defn ^kongra.prelude.Randist randist
  []
  (chRandist @randist-state))

(defn set-seed!
  [^long seed]
  (reset! randist-state (kongra.prelude.Randist. (make-MersenneTwister seed))))

(defmacro randgen!
  [method & args]
  `(~method (randist) ~@args))


;; (defn t1 [x]
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x)
;;   (chLong x))

;; (defn foo [x] (chString x))

;; (defn t2
;;   []
;;   (dotimes [i 1000000] (chString (foo "aaa"))))

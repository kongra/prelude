;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26

(ns kongra.prelude
  (:require [primitive-math :as    p]
            [clojure.set    :as cset]))

;; NON-NULL AND TYPE CHECKS FOR BOXED PRIMITIVES AND STRINGS

(defn ^String chString ;:- String -> String
  {:inline (fn [s] `(kongra.prelude.PrimitiveChecks/chString ~s))}
  [s]
  (kongra.prelude.PrimitiveChecks/chString s))


(defn ^String chLong ;:- Long -> Long
  {:inline (fn [l] `(kongra.prelude.PrimitiveChecks/chLong ~l))}
  [l]
  (kongra.prelude.PrimitiveChecks/chLong l))


(defn ^String chDouble ;:- Double -> Double
  {:inline (fn [d] `(kongra.prelude.PrimitiveChecks/chDouble ~d))}
  [d]
  (kongra.prelude.PrimitiveChecks/chDouble d))


(defn ^String chBoolean ;:- Boolean -> Boolean
  {:inline (fn [b] `(kongra.prelude.PrimitiveChecks/chBoolean ~b))}
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


;; SOME REFLECTION UTILS

(def ^:private COLLREFLECTS
  [["java.util.List"                     #(instance? java.util.List %)]
   ["java.util.Map"                      #(instance? java.util.Map  %)]
   ["java.util.Set"                      #(instance? java.util.Set  %)]

   ["seq? (clojure.lang.ISeq)"                                    seq?]
   ["lazy? (clojure.lang.LazySeq)" #(instance? clojure.lang.LazySeq %)]

   ["sequential? (clojure.lang.Sequential)"                sequential?]
   ["clojure.lang.Seqable"         #(instance? clojure.lang.Seqable %)]
   ["counted? (clojure.lang.Counted)"                         counted?]
   ["clojure.lang.Indexed"         #(instance? clojure.lang.Indexed %)]
   ["associative? (clojure.lang.Associative)"             associative?]
   ["clojure.lang.ILookup"         #(instance? clojure.lang.ILookup %)]
   ["sorted? (clojure.lang.Sorted)"                            sorted?]
   ["reversible? (clojure.lang.Reversible)"                reversible?]

   ["coll? (clojure.lang.IPersistentCollection)"                 coll?]
   ["list? (clojure.lang.IPersistentList)"                       list?]
   ["vector? (clojure.lang.IPersistentVector)"                 vector?]
   ["map? (clojure.lang.IPersistentMap)"                          map?]
   ["set? (clojure.lang.IPersistentSet)"                          set?]

   ["clojure.lang.ASeq"              #(instance? clojure.lang.ASeq %)]])


(defn coll-reflects
  ([coll] ;:- coll -> [String]
   (->> COLLREFLECTS
        (filter (fn [[_ pred]] (pred coll)))
        (map first)))

  ([coll & colls] ;:- coll -> & coll -> #{String}
   (apply cset/intersection (map #(set (coll-reflects %)) (cons coll colls)))))


;; KLEENE LOGIC

(deftype ^:private Kleene [s]
  Object
  (toString [_] (str s)))

(def ^Kleene KleeneTrue      (Kleene. "KleeneTrue"     ))
(def ^Kleene KleeneFalse     (Kleene. "KleeneFalse"    ))
(def ^Kleene KleeneUndefined (Kleene. "KleeneUndefined"))

(defn Kleene-not ;:- Kleene -> Kleene
  [x]
  (let [x (cast Kleene x)]
    (cond (ref= KleeneTrue  x) KleeneFalse
          (ref= KleeneFalse x) KleeneTrue
          :else                KleeneUndefined)))


(defmacro Kleene-and
  ([] KleeneTrue)
  ([x] `(cast Kleene ~x))
  ([x & xs]
   `(let [x# (cast Kleene ~x)]
      (cond (ref= KleeneFalse x#) KleeneFalse
            (ref= KleeneTrue  x#) (Kleene-and ~@xs)
            :else ;; KleeneUndefined
            (if (ref= KleeneFalse (Kleene-and ~@xs))
              KleeneFalse
              KleeneUndefined)))))


(defmacro Kleene-or
  ([]  KleeneFalse)
  ([x] `(cast Kleene ~x))
  ([x & xs]
   `(let [x# (cast Kleene ~x)]
      (cond (ref= KleeneTrue  x#) KleeneTrue
            (ref= KleeneFalse x#) (Kleene-or ~@xs)
            :else ;; KleeneUndefined
            (if (ref= KleeneTrue (Kleene-or ~@xs))
              KleeneTrue
              KleeneUndefined)))))


;; TREE SEARCH ROUTINES INSPIRED BY PAIP , CHAPTER 6.4
;; FOR MORE SEE clongra.search

(defn tree-search
  ;;:- (a) -> (a -> Boolean) -> (a -> (a)) -> ((a) -> (a) -> (a)) -> a|nil
  "Searches state-spaces that have the form of trees. Starts with
  a sequence of states and performs the search according to the
  goal? predicate, generator of nodes adjacent do a given node
  and combiner responsible of adding nodes to the search
  collection of nodes."
  [states goal? adjacent combiner]
  (when (seq states)
    (let [obj (first states)]
      (if (goal? obj)
        obj

        (recur (combiner (adjacent obj) (rest states))
               goal?
               adjacent
               combiner)))))


(defn depth-first-combiner ;:- (a) -> (a) -> (a)
  "The combiner for the depth-first-search."
  [new-nodes states]
  (lazy-cat new-nodes states))


(defn breadth-first-combiner ;:- (a) -> (a) -> (a)
  "The combiner for the breadth-first-search."
  [new-nodes states]
  (lazy-cat states new-nodes))


(defn breadth-first-tree-levels
  ;;:- a -> (a -> (a)) -> ((a))
  "Returns a lazy collection of lazy sequences of nodes belonging
  to subsequent tree levels."
  [start adjacent]
  (->> (list start)
       (iterate #(mapcat adjacent %))
       (take-while seq)))


(defn breadth-first-tree-seq
  "Returns a lazy sequence of tree nodes starting with the passed
  start node where adjacent is a function generating nodes
  adjacent to it's argument.

  Goes on infinitely unless the limiting depth specified."
  ([start adjacent] ;:- a -> (a -> (a)) -> (a)
     (apply concat (breadth-first-tree-levels start adjacent)))

  ([start adjacent depth] ;:- a -> (a -> (a)) -> Positive Long -> (a)
     (->> (breadth-first-tree-levels start adjacent)
          (take depth)
          (reduce concat))))


;; RANDOM UTILS

(defn ^String uuid ;:- -> String
  []
  (.. java.util.UUID randomUUID toString))


(defn ^java.util.Random make-MersenneTwister ;:- long -> Random
  [^long seed]
  (let [bs (byte-array 16)]
    (kongra.prelude.Bits/putLong bs 0 seed)
    (org.uncommons.maths.random.MersenneTwisterRNG. bs)))


(def ^:private randist-state
  (atom (kongra.prelude.Randist. (make-MersenneTwister 0))))


(defn ^kongra.prelude.Randist randist ;:- -> kongra.prelude.Randist
  []
  @randist-state)


(defn set-seed! ;:- long -> nil
  [^long seed]
  (reset! randist-state
          (kongra.prelude.Randist. (make-MersenneTwister seed))) nil)


(defmacro randgen!
  [method & args]
  `(~method (randist) ~@args))

;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-11

(ns kongra.prelude.search
  (:require [kongra.ch      :refer                         :all]
            [kongra.prelude :refer [chPosLong chSeq' lazy-cat']]))

;; TREE SEARCH ROUTINES FROM BY PAIP , CHAPTER 6.4

;; COMBINERS

(deftype Comb [f]
  clojure.lang.IFn
  (invoke [_ nodes new-nodes]
    (chSeq (f (chSeq nodes) (chSeq new-nodes)))))

(defchC chComb Comb)
(defn    cComb [f] (Comb. (chIfn f)))

(def breadth-first-combiner      (cComb    concat))
(def lazy-breadth-first-combiner (cComb lazy-cat'))

(def depth-first-combiner        (cComb #(concat   %2 %1)))
(def lazy-depth-first-combiner   (cComb #(lazy-cat %2 %1)))

;; GOAL

(deftype Goal [f]
  clojure.lang.IFn
  (invoke [_ x] (boolean (f x))))

(defchC chGoal `Goal)
(defn    cGoal [f] (Goal. (chIfn f)))

;; ADJACENCY

(deftype Adjs [f]
  clojure.lang.IFn
  (invoke [_ x] (chSeq (f x))))

(defchC chAdjs Adjs)
(defn    cAdjs [f] (Adjs. (chIfn f)))

;; TREE-SEARCH

(defn tree-search
  [start goal? adjs comb]
  (chGoal goal?) (chAdjs adjs) (chComb comb)
  (chMaybe chSome
    (loop [nodes (list start)]
      (when (seq nodes)
        (let [obj (first nodes)]
          (if (goal? obj)
            obj
            (recur (comb (rest nodes) (adjs obj)))))))))

(defn breadth-first-search
  [start goal? adjs]
  (chMaybe chSome
    (tree-search start
                 (chGoal          goal?)
                 (chAdjs           adjs)
                 breadth-first-combiner)))

(defn depth-first-search
  [start goal? adjs]
  (chMaybe chSome
    (tree-search start
                 (chGoal        goal?)
                 (chAdjs         adjs)
                 depth-first-combiner)))

;; TREE-SEARCH SEQ

(defn breadth-first-tree-levels
  [start adjs]
  (chAdjs adjs)
  (chSeq (->> (list              start)
              (iterate #(mapcat adjs %))
              (map              chSeq')
              (take-while          seq))))

(defn breadth-first-tree-seq
  ([start adjs]
   (chAdjs adjs)
   (chSeq (apply concat (breadth-first-tree-levels start adjs))))

  ([start adjs depth]
   (chAdjs     adjs)
   (chPosLong depth)
   (chSeq (->> (breadth-first-tree-levels start adjs)
               (take  depth)
               (apply concat)))))

;; TESTS

;; (def ADJS
;;   '{a (b c d)
;;     b (e f)
;;     d (g h)
;;     g (i j k)
;;     h (l ł m)
;;     k (o p q)})

;; (defn test0
;;   []
;;   (tree-search
;;    'a
;;    (cGoal #(= % 'p))
;;    (cAdjs #(or (ADJS %) '()))
;;    lazy-breadth-first-combiner))

;; (defn test1
;;   []
;;   (breadth-first-search
;;    'a
;;    (cGoal #(= % 'p))
;;    (cAdjs #(or (ADJS %) '()))))

;; (defn test2
;;   []
;;   (doall (breadth-first-tree-levels 'a (cAdjs #(or (ADJS %) '())))))

;; (defn test3
;;   []
;;   (doall (breadth-first-tree-seq 'a (cAdjs #(or (ADJS %) '())) 0)))

;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-11

(ns kongra.prelude.search
  (:require [kongra.ch      :refer :all]
            [kongra.prelude :refer :all]))

;; TREE SEARCH ROUTINES FROM BY PAIP , CHAPTER 6.4

;; COMBINERS

(deftype Comb [f]
  clojure.lang.IFn
  (invoke [_ nodes new-nodes]
    (chSeq (f (chSeq nodes) (chSeq new-nodes)))))

(defchC chComb Comb)
(defn consComb [f] (Comb. (chIfn f)))

(def breadth-first-combiner      (consComb    concat))
(def lazy-breadth-first-combiner (consComb lazy-cat'))

(def depth-first-combiner        (consComb #(concat   %2 %1)))
(def lazy-depth-first-combiner   (consComb #(lazy-cat %2 %1)))

;; GOAL

(deftype Goal [f]
  clojure.lang.IFn
  (invoke [_ x] (boolean (f x))))

(defchC chGoal `Goal)
(defn consGoal [f] (Goal. (chIfn f)))

;; ADJACENCY

(deftype Adjs [f]
  clojure.lang.IFn
  (invoke [_ x] (chSeq (f x))))

(defchC chAdjs Adjs)
(defn consAdjs [f] (Adjs. (chIfn f)))

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

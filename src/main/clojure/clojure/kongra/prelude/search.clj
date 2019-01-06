;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-11
(ns clojure.kongra.prelude.search
  (:require
   [clojure.kongra.ch
    :refer [chIfn chMaybe chSome chBool
            chSeq chPosLong]]

   [clojure.kongra.prelude
    :refer [lazyCat]]))

;; TREE SEARCH ROUTINES FROM BY PAIP, CHAPTER 6.4

;; COMBINERS
(def breadthFirstCombiner      concat)
(def lazyBreadthFirstCombiner lazyCat)

(def depthFirstCombiner     #(concat   %2 %1))
(def lazyDepthFirstCombiner #(lazy-cat %2 %1))

;; TREE-SEARCH
(defn treeSearch
  [start goal? adjs comb]
  (chIfn goal?)
  (chIfn  adjs)
  (chIfn  comb)
  (chMaybe chSome
           (loop [nodes (list start)]
             (when (seq nodes)
               (let [obj (first nodes)]
                 (if (chBool (goal? obj))
                   obj
                   (recur (chSeq (comb (chSeq (rest nodes))
                                       (chSeq (adjs   obj)))))))))))

(defn breadthFirstSearch
  [start goal? adjs]
  (chIfn goal?)
  (chIfn  adjs)
  (chMaybe chSome
           (treeSearch start goal? adjs breadthFirstCombiner)))

(defn depthFirstSearch
  [start goal? adjs]
  (chIfn goal?)
  (chIfn  adjs)
  (chMaybe chSome
           (treeSearch start goal? adjs depthFirstCombiner)))

;; TREE-SEARCH SEQ
(defn breadthFirstTreeLevels
  [start adjs]
  (chIfn adjs)
  (chSeq (->> (list              start)
              (iterate #(mapcat adjs %))
              (map                chSeq)
              (take-while          seq))))

(defn breadthFirstTreeSeq
  ([start adjs]
   (chIfn adjs)
   (chSeq (apply concat (breadthFirstTreeLevels start adjs))))

  ([start adjs depth]
   (chIfn      adjs)
   (chPosLong depth)
   (chSeq (->> (breadthFirstTreeLevels start adjs)
               (take  depth)
               (apply concat)))))

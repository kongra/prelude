;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-11
(ns clojure.kongra.prelude.search
  (:require
   [clojure.kongra.ch
    :refer [chIfn chMaybe chSome chBool
            chSeq chPosLong]]

   [clojure.kongra.prelude
    :refer [lazy-cat']]))

;; TREE SEARCH ROUTINES FROM BY PAIP, CHAPTER 6.4

;; COMBINERS
(def breadth-first-combiner        concat )
(def lazy-breadth-first-combiner lazy-cat')

(def depth-first-combiner      #(concat   %2 %1))
(def lazy-depth-first-combiner #(lazy-cat %2 %1))

;; TREE-SEARCH
(defn tree-search
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

(defn breadth-first-search
  [start goal? adjs]
  (chIfn goal?)
  (chIfn  adjs)
  (chMaybe chSome
           (tree-search start goal? adjs breadth-first-combiner)))

(defn depth-first-search
  [start goal? adjs]
  (chIfn goal?)
  (chIfn  adjs)
  (chMaybe chSome
           (tree-search start goal? adjs depth-first-combiner)))

;; TREE-SEARCH SEQ
(defn breadth-first-tree-levels
  [start adjs]
  (chIfn adjs)
  (chSeq (->> (list              start)
              (iterate #(mapcat adjs %))
              (map                chSeq)
              (take-while          seq))))

(defn breadth-first-tree-seq
  ([start adjs]
   (chIfn adjs)
   (chSeq (apply concat (breadth-first-tree-levels start adjs))))

  ([start adjs depth]
   (chIfn      adjs)
   (chPosLong depth)
   (chSeq (->> (breadth-first-tree-levels start adjs)
               (take  depth)
               (apply concat)))))

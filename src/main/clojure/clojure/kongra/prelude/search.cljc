;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-11
(ns clojure.kongra.prelude.search)

(set! *warn-on-reflection* true)

;; TREE SEARCH ROUTINES FROM BY PAIP, CHAPTER 6.4

;; COMBINERS
(def breadthFirstCombiner               concat)
(def lazyBreadthFirstCombiner #(lazy-cat %1 %2))

(def depthFirstCombiner       #(concat   %2 %1))
(def lazyDepthFirstCombiner   #(lazy-cat %2 %1))

;; TREE-SEARCH
(defn treeSearch
  [start goal? adjs comb]
  (loop [nodes (list start)]
    (when (seq nodes)
      (let [obj (first nodes)]
        (if (goal? obj)
          obj
          (recur (comb (rest nodes) (adjs obj))))))))

(defn breadthFirstSearch
  [start goal? adjs]
  (treeSearch start goal? adjs breadthFirstCombiner))

(defn depthFirstSearch
  [start goal? adjs]
  (treeSearch start goal? adjs depthFirstCombiner))

;; TREE-SEARCH SEQ
(defn breadthFirstTreeLevels
  [start adjs]
  (->>
    (list              start)
    (iterate #(mapcat adjs %))
    (take-while          seq)))

(defn breadthFirstTreeSeq
  ([start adjs]
   (apply concat (breadthFirstTreeLevels start adjs)))

  ([start adjs depth]
   (->>
     (breadthFirstTreeLevels start adjs)
     (take   depth)
     (apply concat))))

;; PERF. TEST
;; (defn evenAdjs
;;   [n i]
;;   (take n (filter even? (iterate inc (inc i)))))

;; (defn oddAdjs
;;   [n i]
;;   (take n (filter odd? (iterate inc (inc i)))))

;; (defn test1
;;   [n m]
;;   (let [adjs  #(if (even? %) (oddAdjs n %) (evenAdjs n %))
;;         goal? #(= % m)]

;;     (breadthFirstSearch 0 goal? adjs)))

;; (test1 200 430)
;; (quick-bench (test1 20 79))
;; (quick-bench (test1 200 430))

;; (use 'criterium.core)
;; (quick-bench (test1 20 75))
;; (quick-bench (test1 20 1000000))

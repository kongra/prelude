;; Copyright (c) 2017-present Konrad Grzanek
;; Created 2017-04-07
(ns clojure.kongra.prelude.print
  (:require
   [primitive-math
    :as p])

  (:require
   [clojure.kongra.ch
    :refer [chString chBool chOptional chSequential chIfn
            chNatLong chUnit chAtom]]

   [clojure.kongra.prelude
    :refer [markLast]]))

(set! *warn-on-reflection* true)

;; TREE-PRINTING
(def ^:private PRINT-TREE-INDENT       "│   ")
(def ^:private PRINT-TREE-EMPTYINDENT  "    ")
(def ^:private PRINT-TREE-FORCHILD     "├── ")
(def ^:private PRINT-TREE-FORLASTCHILD "└── ")
(def ^:private PRINT-TREE-EOL          "\n"   )
(def ^:private PRINT-TREE-EMPTY        ""    )

(defn ^:private indentSymbol
  [isEmpty?]
  (chString
   (if (chBool isEmpty?)
     PRINT-TREE-EMPTYINDENT
     PRINT-TREE-INDENT)))

(defn ^:private genindent
  [[isLast? & lastChildInfos]]
  (chString
   (do (chBool  isLast?)
       (chOptional chSequential lastChildInfos)
       (let [suffix (if isLast?
                      PRINT-TREE-FORLASTCHILD
                      PRINT-TREE-FORCHILD)
             prefix (->> lastChildInfos
                         butlast
                         reverse
                         (map indentSymbol)
                         (apply str))]

         (str prefix suffix)))))

(defn ^:private printTreeImpl
  [node adjs show ^Long depth ^Long level lastChildInfos isFirst?]
  (chUnit
   (do (chIfn                  adjs)
       (chIfn                  show)
       (chNatLong             depth)
       (chNatLong             level)
       (chSequential lastChildInfos)
       (chBool             isFirst?)
       (let [s    (chString (show node))
             pfx  (if isFirst? PRINT-TREE-EMPTY PRINT-TREE-EOL)
             repr (if (p/zero? (.longValue level))
                    (str pfx s)
                    (str pfx (genindent lastChildInfos) s))]

         (print repr)

         (when-not (p/== (.longValue level) (.longValue depth))
           (let [nextLevel (p/inc level)
                 children   (chSequential (adjs node))]
             (doseq [[child isLast?] (map vector children (markLast children))]
               (printTreeImpl child adjs show depth nextLevel
                              (cons isLast? lastChildInfos) false))))))))

(defn printTree
  "Prints a tree using a textual representation like in UNIX tree command.
   adjs : node -> [node]
   show : node -> String"
  ([node adjs]
   (chUnit (printTree node adjs str)))

  ([node adjs show]
   (chUnit (printTree node adjs show Long/MAX_VALUE)))

  ([node adjs show ^Long depth]
   (chUnit (printTreeImpl node adjs show depth 0 '(true) true))))

;; GRAPH TREE-PRINTING
(deftype ^:private PrintGraphEllipsis [v])

(def ^{:dynamic true :private true} *printGraphVisited*
  (chAtom (atom #{})))

(defn ^:private printGraphShow
  [show v]
  (chString (if (instance? PrintGraphEllipsis v)
              (str (chString (show (.v ^PrintGraphEllipsis v))) " ...")

              (do
                (swap! *printGraphVisited* conj v)
                (show v)))))

(defn ^:private printGraphAdjs
  [adjs v]
  (chSequential
   (if-not (instance? PrintGraphEllipsis v)
     (map
      #(if (@*printGraphVisited* %) (PrintGraphEllipsis. %) %)
      (adjs v))

     ;; no adjs for ellipsis
     '())))

(defn printGraph
  ([v adjs show ^Long depth]
   (chUnit (binding [*printGraphVisited* (chAtom (atom #{}))]
             (printTree v
                        (partial printGraphAdjs adjs)
                        (partial printGraphShow show)
                        depth))))
  ([v adjs show]
   (chUnit (printGraph v adjs show Long/MAX_VALUE)))

  ([v adjs]
   (chUnit (printGraph v adjs str))))

;; SOME TESTS
;; (def t1 '(a (b (e f (l ł m n) g)) (c (h (o p q) i)) (d (j (r s t) (k)))))
;; (defn adjs1 [x]      (if (symbol? x) '() (rest  x)))
;; (defn show1 [x] (str (if (symbol? x)  x  (first x))))
;; (defn test-1 [] (with-out-str (printTree  t1 adjs1 str)))
;; (defn test-2 [] (with-out-str (printGraph t1 adjs1 str)))

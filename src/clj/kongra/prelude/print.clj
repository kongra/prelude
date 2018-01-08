;; Copyright (c) 2017-present Konrad Grzanek
;; Created 2017-04-07

(ns kongra.prelude.print
  (:require [primitive-math :as       p])
  (:require [kongra.ch      :refer :all])
  (:require [kongra.prelude :refer :all]))

;; TREE-PRINTING

(def ^:private PRINT-TREE-INDENT       "│   ")
(def ^:private PRINT-TREE-EMPTYINDENT  "    ")
(def ^:private PRINT-TREE-FORCHILD     "├── ")
(def ^:private PRINT-TREE-FORLASTCHILD "└── ")
(def ^:private PRINT-TREE-EOL          "\n"   )
(def ^:private PRINT-TREE-EMPTY        ""    )

(defn ^:private print-tree-indent-symbol
  [is-empty]
  (chString (if (chBoolean is-empty)
              PRINT-TREE-EMPTYINDENT
              PRINT-TREE-INDENT)))

(defn ^:private print-tree-genindent
  [[is-last & last-child-infos]]
  (chBoolean                  is-last)
  (chMaybe chSequential last-child-infos)
  (chString
   (let [suffix (if is-last PRINT-TREE-FORLASTCHILD PRINT-TREE-FORCHILD)
         prefix (->> last-child-infos
                     butlast
                     reverse
                     (map print-tree-indent-symbol)
                     (apply str))]
     (str prefix suffix))))

(defn ^:private print-tree-impl
  [node adjs show ^Long depth ^Long level last-child-infos is-first]
  (chIfn                    adjs)
  (chIfn                    show)
  (chNatLong               depth)
  (chNatLong               level)
  (chSequential last-child-infos)
  (chBoolean            is-first)
  (chUnit
   (let [s    (chString (show node))
         pfx  (if is-first PRINT-TREE-EMPTY PRINT-TREE-EOL)
         repr (if (p/zero? (.longValue level))
                (str pfx s)
                (str pfx (print-tree-genindent last-child-infos) s))]

     (print repr)

     (when-not (p/== (.longValue level) (.longValue depth))
       (let [next-level (p/inc level)
             children   (chSequential (adjs node))]
         (doseq [[child is-last] (map vector children (mark-last children))]
           (print-tree-impl child adjs show depth next-level
                            (cons is-last last-child-infos) false)))))))

(defn print-tree
  "Prints a tree using a textual representation like in UNIX tree command.
  adjs : node -> [node]
  show     : node -> String"
  ([node adjs]
   (chIfn adjs)
   (chUnit (print-tree node adjs str)))

  ([node adjs show]
   (chIfn adjs)
   (chIfn show)
   (chUnit (print-tree node adjs show Long/MAX_VALUE)))

  ([node adjs show depth]
   (chIfn      adjs)
   (chIfn      show)
   (chNatLong depth)
   (chUnit (print-tree-impl node adjs show depth 0 '(true) true))))

;; GRAPH TREE-PRINTING

(deftype ^:private PrintGraphEllipsis [v])

(def ^{:dynamic true :private true} *print-graph-visited*
  (chAtom (atom #{})))

(defn ^:private print-graph-show
  [show v]
  (chIfn show)
  (chString (if (instance? PrintGraphEllipsis v)
              (str (chString (show (.v ^PrintGraphEllipsis v))) " ...")

              (do
                (swap! *print-graph-visited* conj v)
                (show v)))))

(defn ^:private print-graph-adjs
  [adjs v]
  (chIfn adjs)
  (chSequential
   (if-not (instance? PrintGraphEllipsis v)
     (map
      #(if (@*print-graph-visited* %) (PrintGraphEllipsis. %) %)
      (adjs v))

     ;; no adjs for ellipsis
     '())))

(defn print-graph
  ([v adjs show depth]
   (chIfn      adjs)
   (chIfn      show)
   (chNatLong depth)
   (chUnit (binding [*print-graph-visited* (chAtom (atom #{}))]
             (print-tree v
                         (partial print-graph-adjs adjs)
                         (partial print-graph-show show)
                         depth))))
  ([v adjs show]
   (chIfn adjs)
   (chIfn show)
   (chUnit (print-graph v adjs show Long/MAX_VALUE)))

  ([v adjs]
   (chIfn adjs)
   (chUnit (print-graph v adjs str))))

;; SOME TESTS
;; (def t1 '(a (b (e f (l ł m n) g)) (c (h (o p q) i)) (d (j (r s t) (k)))))
;; (defn adjs1 [x]      (if (symbol? x) '() (rest  x)))
;; (defn show1 [x] (str (if (symbol? x)  x  (first x))))
;; (defn test-1 [] (with-out-str (print-tree  t1 adjs1 str)))
;; (defn test-2 [] (with-out-str (print-graph t1 adjs1 str)))

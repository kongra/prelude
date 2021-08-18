(set! *warn-on-reflection* true)

(defn foo-1
  [n]
  (reduce + 0 (range n)))

(defn foo
  [n]
  (loop [i 0
         result 0]

    (if (= i n)
      result

      (recur (inc i) (+ result i)))))

(defn foo-2 ^long
  [^long n]
  (loop [i 0
         result 0]

    (if (= i n)
      result

      (recur (inc i) (+ result i)))))

(require '[criterium.core :refer [quick-bench]])
;; (require '[no.disassemble :refer [disassemble]])

(quick-bench (foo 10000000))
(quick-bench (foo-2 10000000))

;; (println (disassemble foo))

;; (time
;;   (dotimes [i 1000]
;;     (foo-2 1000000)))

#_(when (not (= i 4))
    (do-sthing))

(if (not <predicate>)
  ()

  ())

unless ... then
  ...

unless <pred> === when not <pred>
   ...               ...

;; 1st take:
(defn unless
  [pred body]
  (when (not pred)
    (body)))

(def i 5)

(unless (= i 4)
  #(print "It works"))

;; 2nd take
(defmacro unless
  [pred & body]
  (println "--1" pred)
  (println "--2" body)

  `(when (not ~pred)
     ~@body))

(unless (= i 4)
  (print "It works ")
  (print "Cause I'm great"))

(defmacro with-sthing
  [expr & body]
  `(let [~'it ~expr]
     ~@body))

(with-sthing (+ 1 2 3 4 5)
  (print it)
  (+ 10  it))

(when (= i 4)
  (print i))

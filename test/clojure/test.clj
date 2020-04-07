(defn fibcount
  [n]
  (if (or (zero? n) (= 1 n))
    [n 1]

    (let [[v1 c1] (fibcount (- n 1))
          [v2 c2] (fibcount (- n 2))]
      [(+ v1 v2) (+ c1 c2 1)])))

;; (time (do (println (fibcount 41))
;;           (println (fibcount 42))
;;           (println (fibcount 43))))

(defn fibcount1
  [n]
  (if (or (zero? n) (= 1 n))
    {:v n :c 1}

    (let [r1 (fibcount1 (- n 1))
          v1 (:v r1)
          c1 (:c r1)

          r2 (fibcount1 (- n 2))
          v2 (:v r2)
          c2 (:c r2)]

      {:v (+ v1 v2) :c (+ c1 c2 1)})))

;; (time (do (println (fibcount1 41))
;;           (println (fibcount1 42))
;;           (println (fibcount1 43))))

(defrecord FibCount2 [v c])
(defn fibcount2
  [n]
  (if (or (zero? n) (= 1 n))
    (FibCount2. n 1)

    (let [r1 (fibcount2 (- n 1))
          v1 (:v r1)
          c1 (:c r1)

          r2 (fibcount2 (- n 2))
          v2 (:v r2)
          c2 (:c r2)]

      (FibCount2. (+ v1 v2) (+ c1 c2 1)))))

;; (time (do (println (fibcount2 41))
;;           (println (fibcount2 42))
;;           (println (fibcount2 43))))

(deftype FibCount3 [^long v ^long c])
(defn ^FibCount3 fibcount3
  [^long n]
  (if (or (zero? n) (= 1 n))
    (FibCount3. n 1)

    (let [r1 (fibcount3 (- n 1))
          v1 (.v r1)
          c1 (.c r1)

          r2 (fibcount3 (- n 2))
          v2 (.v r2)
          c2 (.c r2)]

      (FibCount3. (+ v1 v2) (+ c1 c2)))))

;; (time (do (println (fibcount3 41))
;;           (println (fibcount3 42))
;;           (println (fibcount3 43))))

(defn fibo-loop ^long
  [^long a ^long b ^long n]
  (if (zero? n)
    a
    (recur b (+ a b) (dec n))))

(defn fibo ^long
  [^long n]
  (fibo-loop 0 1 n))

(time
  (dotimes [i 10000000]
    (fibo 78)))

(time
  (dotimes [i 10000000]
    (fibo 78)))

(time
  (dotimes [i 10000000]
    (fibo 78)))

;; (use 'no.disassemble)
;; (println (disassemble fibo-loop))

(in-ns 'user)

(defn lr [^long value]
  (long-array 1 value))

(defn val ^long
  [^longs lr]
  (aget lr 0))

(defn setf!
  [^longs lr ^long v]
  (aset lr 0 v))

(defn make-counter
  [^long start]
  (let [value (lr (dec start))]
    (fn []
      (setf! value (inc (val value))))))

(defn sum-counter ^long
  [^long n]
  (let [c1 (make-counter n)
        result (lr 0)]

    (dotimes [i 10]
      (let [m (c1)]
        (setf! result (+ (val result) m))))

    (val result)))

(defn test-1 [n] ^long
  (let [result (lr 0)]
    (dotimes [i 100000000]
      (setf! result (+ (val result) (sum-counter (+ i n)))))

    (val result)))

(time (dotimes [i 10] (println (test-1 i))))

(set! *warn-on-reflection* true)

(defn fast-long-array [^clojure.lang.PersistentVector xs ^long n]
  (let [arr (long-array n)]
    (dotimes [i n]
      (let [v1 (.longValue ^Long (.get xs i))]
        (aset arr i v1)))

    arr))

(defn minimum-swaps ^long
  [^clojure.lang.PersistentVector xs]
  (let [n                (.length         xs)
        elements  ^longs (fast-long-array xs n)
        positions        (long-array  n)]

    ;; Fill-in positions
    (dotimes [i n]
      (aset positions (aget ^longs elements i) i))

    ;; Do the actual counting
    (loop [i 0
           num-swaps 0]

      (if (= i n)
        num-swaps

        (let [el (aget elements i)]
          (if (= el i)
            (recur (unchecked-inc i) num-swaps)

            (let [despos (aget elements i)]
              (aset positions despos el)
              (aset elements  el despos)
              (recur (unchecked-inc i) (unchecked-inc num-swaps)))))))))

;; (require '[no.disassemble :refer [disassemble]])
;; (println (disassemble fast-long-array))
;; (println (disassemble minimum-swaps))

;; (require '[criterium.core :refer [quick-bench]])
;; (def xs1 (vec (reverse (range   10))))
;; (def xs2 (vec (reverse (range  100))))
;; (def xs3 (vec (reverse (range 1000))))
;; (quick-bench (minimum-swaps xs1))
;; (quick-bench (minimum-swaps xs2))
;; (quick-bench (minimum-swaps xs3))

(require '[clojure.pprint :refer [pprint]])

(->> (java.lang.management.ManagementFactory/getRuntimeMXBean)
  (.getInputArguments)
  (into [])
  (pprint))

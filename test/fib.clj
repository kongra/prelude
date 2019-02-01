(use 'clojure.kongra.ch)

(deftype Fib
    [^long value
     ^long count])

(def chFib (chP (instance? Fib x)))

(defn fibcount ^Fib
  [^long n]
  (chFib
   (do (chNatLong n)
       (if (or (= 0 n) (= 1 n))
         (Fib. n 1)

         (let [c1 (fibcount (- n 1))
               c2 (fibcount (- n 2))]
           (Fib. (+ (.value c1) (.value c2))
                 (+ (.count c1) (.count c2) 1)))))))

(defn test1
  [^long n]
  (let [f (fibcount n)]
    (println (.value f) (.count f))))

#_(use 'criterium.core)
#_(use 'no.disassemble)
#_(println (disassemble fibcount))

#_(time (do (test1 42)
            (test1 43)
            (test1 44)))

(defn fib ^long
  [^long n]
  (chNatLong
   (do (chNatLong n)
       (if (or (= n 0) (= n 1))
         n

         (+ (fib (- n 1)) (fib (- n 2)))))))

(time (do (println 43 (fib 43))
          (println 44 (fib 44))
          (println 45 (fib 45))))

(println (disassemble fib))

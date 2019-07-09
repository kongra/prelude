
























#_(defn test1
    []
    (println "Idę spać...")
    (Thread/sleep 30000)
    (println "... koniec.")
    77)

#_(println "Wywołuję (test1)")
#_(def f1 (future (test1)))
#_(println "Po wywołaniu (test1)")

;; (def running? (atom true))

;; (defn test2
;;   []
;;   (while @running?
;;     (println "Działa test2")
;;     (Thread/sleep 1000)))

;; (future (test2))
;; (reset! running? false)

#_(def acc1 (atom  5000))
#_(def acc2 (atom 10000))

;; (def acc1 (ref  5000))
;; (def acc2 (ref 10000))

;; (defn sleepTime
;;   []
;;   (long (* 10 (java.lang.Math/random))))

;; (defn runme
;;   []
;;   (dotimes [i 1000]
;;     (dosync
;;      (let [v2 @acc2
;;            v1 @acc1]
;;        (ref-set acc2 (- v2 1))
;;        (Thread/sleep (sleepTime))
;;        (ref-set acc1 (+ v1 1)))))

;;   (println "Koniec."))

;; (future (runme))
;; (future (runme))

;; (println "Diagn:" (+ @acc1 @acc2))

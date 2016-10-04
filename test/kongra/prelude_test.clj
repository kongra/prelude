;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-03

(ns kongra.prelude-test
  (:require [clojure.test   :refer :all]
            [kongra.prelude :refer :all]))

(deftype A []) (defchc chA A)
(deftype B []) (defchc chB B)
(deftype C []) (defchc chC C)

(defch chMaybeA      (chp (chMaybe  chA       )))
(defch chEitherAUnit (chp (chEither chA chUnit)))
(defch chEitherAB    (chp (chEither chA    chB)))
(defch chABC         (chp (ch| [chA chB chC]  )))

(deftest ch-test
  (testing "(ch ...)"
    (is (thrown? AssertionError      (ch  (nil?)     1)))
    (is (nil?                        (ch  (nil?)   nil)))
    (is (false?                 (chp (ch  (nil?))    1)))
    (is (true?                  (chp (ch  (nil?)) nil))))

  (testing "(chc ...)"
    (is (= ""                        (chc String    "")))
    (is (thrown? AssertionError      (chc String     1)))
    (is (thrown? AssertionError      (chc String   nil)))
    (is (true?                  (chp (chc String)   "")))
    (is (false?                 (chp (chc String)    1)))
    (is (false?                 (chp (chc String) nil))))

  (testing "(defchc ...)"
    (is (chA (A.)))
    (is (thrown? AssertionError (chA                   1)))
    (is (thrown? AssertionError (chA                 nil)))
    (is (true?                  (chA #_ as-pred nil (A.))))
    (is (false?                 (chA #_ as-pred nil    1)))
    (is (false?                 (chA #_ as-pred nil nil))))

  (testing "(chLike ...)"
    (is (chLike 1 2))
    (is (thrown? AssertionError (chLike 1     "aaa")))
    (is (thrown? AssertionError (chLike "aaa"     2)))
    (is (thrown? AssertionError (chLike 1       nil)))

    (is (true?  (chLike 2/3   #_ as-pred nil   3/4)))
    (is (false? (chLike 1     #_ as-pred nil "aaa")))
    (is (false? (chLike "aaa" #_ as-pred nil     2)))
    (is (false? (chLike 1     #_ as-pred nil   nil))))

  (testing "(chUnit ...)"
    (is (nil?                   (chUnit nil)))
    (is (thrown? AssertionError (chUnit   1)))

    (is (true?  (chUnit #_ as-pred nil nil)))
    (is (false? (chUnit #_ as-pred nil ""))))

  (testing "(chSome ...)"
    (is                         (chSome   1))
    (is (thrown? AssertionError (chSome nil)))

    (is (true?  (chSome #_ as-pred nil  "")))
    (is (false? (chSome #_ as-pred nil nil))))

  (testing "(chMaybe ...)"
    (is (nil?                   (chMaybe chA                 nil)))
    (is                         (chMaybe chA                 (A.)))
    (is (thrown? AssertionError (chMaybe chA                (B.))))
    (is (true?                  (chMaybe chA #_ as-pred nil  nil)))
    (is (true?                  (chMaybe chA #_ as-pred nil (A.))))
    (is (false?                 (chMaybe chA #_ as-pred nil (B.))))

    (is (nil?                   (chMaybe chUnit nil)))
    (is (thrown? AssertionError (chMaybe chUnit (A.))))
    (is (thrown? AssertionError (chMaybe chUnit (B.)))))

  (testing "(chMaybe ...) with (defch ...)"
    (is (nil?                   (chMaybeA                 nil)))
    (is                         (chMaybeA                 (A.)))
    (is (thrown? AssertionError (chMaybeA                (B.))))
    (is (true?                  (chMaybeA #_ as-pred nil  nil)))
    (is (true?                  (chMaybeA #_ as-pred nil (A.))))
    (is (false?                 (chMaybeA #_ as-pred nil (B.)))))

  (testing "(chEither ...)"
    (is (nil?                   (chEither chA chUnit  nil)))
    (is                         (chEither chA chUnit  (A.)))
    (is (thrown? AssertionError (chEither chA chUnit (B.))))
    (is                         (chEither chA chB     (A.)))
    (is                         (chEither chA chB     (B.)))
    (is (thrown? AssertionError (chEither chA chB    (C.))))
    (is (thrown? AssertionError (chEither chA chB     nil)))

    (is (true?   (chEither chA chUnit #_ as-pred nil  nil)))
    (is (true?   (chEither chA chUnit #_ as-pred nil (A.))))
    (is (false?  (chEither chA chUnit #_ as-pred nil (B.))))
    (is (true?   (chEither chA chB    #_ as-pred nil (A.))))
    (is (true?   (chEither chA chB    #_ as-pred nil (B.))))
    (is (false?  (chEither chA chB    #_ as-pred nil (C.))))
    (is (false?  (chEither chA chB    #_ as-pred nil nil))))

  (testing "(chEither ...) with (defch ...)"
    (is (nil?                   (chEitherAUnit  nil)))
    (is                         (chEitherAUnit  (A.)))
    (is (thrown? AssertionError (chEitherAUnit (B.))))
    (is                         (chEitherAB     (A.)))
    (is                         (chEitherAB     (B.)))
    (is (thrown? AssertionError (chEitherAB    (C.))))
    (is (thrown? AssertionError (chEitherAB     nil)))

    (is (true?   (chEitherAUnit #_ as-pred nil  nil)))
    (is (true?   (chEitherAUnit #_ as-pred nil (A.))))
    (is (false?  (chEitherAUnit #_ as-pred nil (B.))))
    (is (true?   (chEitherAB    #_ as-pred nil (A.))))
    (is (true?   (chEitherAB    #_ as-pred nil (B.))))
    (is (false?  (chEitherAB    #_ as-pred nil (C.))))
    (is (false?  (chEitherAB    #_ as-pred nil nil))))

  (testing "(ch| ...)"
    (is                         (ch| [chA chB chC]   (A.)))
    (is                         (ch| [chA chB chC]   (B.)))
    (is                         (ch| [chA chB chC]   (C.)))
    (is (thrown? AssertionError (ch| [chA chB chC]   nil)))
    (is (thrown? AssertionError (ch| [chA chB chC] "aaa")))
    (is (thrown? AssertionError (ch| [chA chB chC]     1)))

    (is (true?   (ch| [chA chB chC] #_ as-pred nil  (A.))))
    (is (true?   (ch| [chA chB chC] #_ as-pred nil  (B.))))
    (is (true?   (ch| [chA chB chC] #_ as-pred nil  (C.))))
    (is (false?  (ch| [chA chB chC] #_ as-pred nil   nil)))
    (is (false?  (ch| [chA chB chC] #_ as-pred nil "aaa")))
    (is (false?  (ch| [chA chB chC] #_ as-pred nil     1)))

    (is                         (ch| [chA chC chB]   (A.)))
    (is                         (ch| [chA chC chB]   (B.)))
    (is                         (ch| [chA chC chB]   (C.)))
    (is (thrown? AssertionError (ch| [chA chC chB]   nil)))
    (is (thrown? AssertionError (ch| [chA chC chB] "aaa")))
    (is (thrown? AssertionError (ch| [chA chC chB]     1)))

    (is                         (ch| [chB chA chC]   (A.)))
    (is                         (ch| [chB chA chC]   (B.)))
    (is                         (ch| [chB chA chC]   (C.)))
    (is (thrown? AssertionError (ch| [chB chA chC]   nil)))
    (is (thrown? AssertionError (ch| [chB chA chC] "aaa")))
    (is (thrown? AssertionError (ch| [chB chA chC]     1)))

    (is                         (ch| [chB chC chA]   (A.)))
    (is                         (ch| [chB chC chA]   (B.)))
    (is                         (ch| [chB chC chA]   (C.)))
    (is (thrown? AssertionError (ch| [chB chC chA]   nil)))
    (is (thrown? AssertionError (ch| [chB chC chA] "aaa")))
    (is (thrown? AssertionError (ch| [chB chC chA]     1)))

    (is                         (ch| [chC chA chB]   (A.)))
    (is                         (ch| [chC chA chB]   (B.)))
    (is                         (ch| [chC chA chB]   (C.)))
    (is (thrown? AssertionError (ch| [chC chA chB]   nil)))
    (is (thrown? AssertionError (ch| [chC chA chB] "aaa")))
    (is (thrown? AssertionError (ch| [chC chA chB]     1)))

    (is                         (ch| [chC chB chA]   (A.)))
    (is                         (ch| [chC chB chA]   (B.)))
    (is                         (ch| [chC chB chA]   (C.)))
    (is (thrown? AssertionError (ch| [chC chB chA]   nil)))
    (is (thrown? AssertionError (ch| [chC chB chA] "aaa")))
    (is (thrown? AssertionError (ch| [chC chB chA]    1))))

  (testing "(ch| ...) with (defch ...)"
    (is                         (chABC   (A.)))
    (is                         (chABC   (B.)))
    (is                         (chABC   (C.)))
    (is (thrown? AssertionError (chABC   nil)))
    (is (thrown? AssertionError (chABC "aaa")))
    (is (thrown? AssertionError (chABC     1)))

    (is (true?   (chABC #_ as-pred nil  (A.))))
    (is (true?   (chABC #_ as-pred nil  (B.))))
    (is (true?   (chABC #_ as-pred nil  (C.))))
    (is (false?  (chABC #_ as-pred nil   nil)))
    (is (false?  (chABC #_ as-pred nil "aaa")))
    (is (false?  (chABC #_ as-pred nil   1)))))

(time (run-tests))

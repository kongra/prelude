;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-10-03

(ns kongra.prelude-test
  (:require [clojure.test   :refer :all]
            [kongra.prelude :refer :all]))

(deftype A []) (defchc chA A)
(deftype B []) (defchc chB B)
(deftype C []) (defchc chC C)

(deftest ch-test
  (testing "(ch ...)"
    (is (thrown? AssertionError (ch (nil?)                    1)))
    (is (nil?                   (ch (nil?)                  nil)))
    (is (false?                 (ch (nil?) #_ as-pred nil     1)))
    (is (true?                  (ch (nil?) #_ as-pred nil  nil))))

  (testing "(chc ...)"
    (is (= ""                   (chc String                  "")))
    (is (thrown? AssertionError (chc String                   1)))
    (is (thrown? AssertionError (chc String                 nil)))
    (is (true?                  (chc String #_ as-pred nil   "")))
    (is (false?                 (chc String #_ as-pred nil    1)))
    (is (false?                 (chc String #_ as-pred nil nil))))

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

  (testing "(chObj ...)"
    (is                         (chObj   1))
    (is (thrown? AssertionError (chObj nil)))

    (is (true?  (chObj #_ as-pred nil  "")))
    (is (false? (chObj #_ as-pred nil nil))))

  (testing "(chMaybe ...)")

  (testing "(chMaybe ...) with (defch ...)")

  (testing "(chEither ...)")

  (testing "(chEither ...) with (defch ...)")

  (testing "(ch| ...)")

  (testing "(ch| ...) with (defch ...)"))

(time (run-tests))

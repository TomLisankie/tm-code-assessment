(ns tm-code-assessment.second-chance-test
  (:require  [clojure.test :refer :all]
             [tm-code-assessment.second-chance :refer :all]))

(deftest valid-lock-screen-pattern
  (testing "function to detect valid lock screen patterns works properly"
    (is (true?  (valid-path [1 6 7 4])))   ;; knights jump is valid
    (is (true?  (valid-path [2 1 3])))     ;; 2 is already used, so we can cross it
    (is (false? (valid-path [1 3 2])))     ;; can't get from 1 to 3 without using 2 first
    (is (false? (valid-path [1 9])))       ;; can't cross 5 without using
    (is (false? (valid-path [1 2 3 2 1]))) ;; can't use dots more than once
    (is (false? (valid-path [0 1 2 3]))))) ;; there's no dot 0

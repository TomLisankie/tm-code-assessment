(ns tm-code-assessment.core-test
  (:require [clojure.test :refer :all]
            [tm-code-assessment.core :refer :all]))

(deftest tic-tac-toe
  (testing "tic-tac-toe winner checker works properly"
    (is (= :x (tic-tac-toe-winner [
                                   [:x :o :x]
                                   [:x :o :o]
                                   [:x :x :o]])))
    (is (= :o (tic-tac-toe-winner [
                                   [:o :x :x]
                                   [:x :o :x]
                                   [:x :o :o]])))
    (is (nil? (tic-tac-toe-winner [
                                   [:x :o :x]
                                   [:x :o :x]
                                   [:o :x :o]])))))

(deftest valid-lock-screen-pattern
  (testing "function to detect valid lock screen patterns works properly"
    (is (true?  (valid-passcode-pattern? [1 6 7 4])))   ;; knights jump is valid
    (is (true?  (valid-passcode-pattern? [2 1 3])))     ;; 2 is already used, so we can cross it
    (is (false? (valid-passcode-pattern? [1 3 2])))     ;; can't get from 1 to 3 without using 2 first
    (is (false? (valid-passcode-pattern? [1 9])))       ;; can't cross 5 without using
    (is (false? (valid-passcode-pattern? [1 2 3 2 1]))) ;; can't use dots more than once
    (is (false? (valid-passcode-pattern? [0 1 2 3]))))) ;; there's no dot 0

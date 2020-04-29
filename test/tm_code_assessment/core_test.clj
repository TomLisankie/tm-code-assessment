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

(deftest occurrences-of-word-in-word-search-puzzle
  (testing "function to count how many occurrences of a word are in a matrix consisting of alphabetic characters"
    (let [test-puzzle [[\A \O \T \D \L \R \O \W]
                       [\L \C \B \M \U \M \L \U]
                       [\D \R \U \J \D \B \L \J]
                       [\P \A \Z \H \Z \Z \E \F]
                       [\B \C \Z \E \L \F \H \W]
                       [\R \K \U \L \V \P \P \G]
                       [\A \L \B \L \P \O \P \Q]
                       [\B \E \M \O \P \P \J \Y]]]
      (is (= 2 (occurrences-of-word-in-grid test-puzzle "HELLO")))
      (is (= 1 (occurrences-of-word-in-grid test-puzzle "WORLD")))
      (is (= 2 (occurrences-of-word-in-grid test-puzzle "BUZZ")))
      (is (= 0 (occurrences-of-word-in-grid test-puzzle "CLOJURE")))
      (is (= 0 (occurrences-of-word-in-grid test-puzzle "COWABUNGA"))))))

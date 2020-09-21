(ns tm-code-assessment.second-chance-test
  (:require  [clojure.test :refer :all]
             [tm-code-assessment.second-chance :refer :all]))

(deftest tic-tac-toe-test
  (testing "function that finds who won a game of tic-tac-toe"
    (is (= :x (get-tic-tac-toe-winner [[:x :o :x] [:x :o :o] [:x :x :o]])))
    (is (= :o (get-tic-tac-toe-winner [[:o :x :x] [:x :o :x] [:x :o :o]])))
    (is (nil? (get-tic-tac-toe-winner [[:x :o :x] [:x :o :x] [:o :x :o]])))))

(deftest valid-lock-screen-pattern
  (testing "function to detect valid lock screen patterns works properly"
    (is (true?  (valid-path [1 6 7 4])))   ;; knights jump is valid
    (is (true?  (valid-path [2 1 3])))     ;; 2 is already used, so we can cross it
    (is (true?  (valid-path [5 3 7])))
    (is (false? (valid-path [1 3 2])))     ;; can't get from 1 to 3 without using 2 first
    (is (false? (valid-path [1 9])))       ;; can't cross 5 without using
    (is (false? (valid-path [1 2 3 2 1]))) ;; can't use dots more than once
    (is (false? (valid-path [0 1 2 3]))))) ;; there's no dot 0

(def test-puzzle
  [[\A \O \T \D \L \R \O \W]
   [\L \C \B \M \U \M \L \U]
   [\D \R \U \J \D \B \L \J]
   [\P \A \Z \H \Z \Z \E \F]
   [\B \C \Z \E \L \F \H \W]
   [\R \K \U \L \V \P \P \G]
   [\A \L \B \L \P \O \P \Q]
   [\B \E \M \O \P \P \J \Y]])

(deftest occurrences-of-word-in-word-search-puzzle
  (testing "function to count how many occurrences of a word are in a matrix consisting of alphabetic characters"
    (is (= 2 (count-words-in-matrix test-puzzle "HELLO")))
    (is (= 1 (count-words-in-matrix test-puzzle "WORLD")))
    (is (= 2 (count-words-in-matrix test-puzzle "BUZZ")))
    (is (= 0 (count-words-in-matrix test-puzzle "CLOJURE")))
    (is (= 0 (count-words-in-matrix test-puzzle "COWABUNGA")))))

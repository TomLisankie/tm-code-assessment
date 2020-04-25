(ns tm-code-assessment.core-test
  (:require [clojure.test :refer :all]
            [tm-code-assessment.core :refer :all]))

(deftest tic-tac-toe
  (testing "tic-tac-toe checker works properly"
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

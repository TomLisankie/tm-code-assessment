(ns tm-code-assessment.core-test
  (:require [clojure.test :refer :all]
            [tm-code-assessment.core :refer :all]))

(deftest tic-tac-toe
  (testing "tic-tac-toe checker works properly"
    (is (= :x (ttt [
                    [:x :o :x]
                    [:x :o :o]
                    [:x :x :o]])))
    (is (= :o (ttt [
                    [:o :x :x]
                    [:x :o :x]
                    [:x :o :o]])))
    (is (nil? (ttt [
                    [:x :o :x]
                    [:x :o :x]
                    [:o :x :o]])))))

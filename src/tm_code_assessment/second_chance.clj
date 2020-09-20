(ns tm-code-assessment.second-chance)

;; PROBLEM 1

(defn- check-for-win
  [& win-types]
  (let [has-win-situation? (fn [win-type]
                             (let [matches (first (filter #(or (= % #{:x}) (= % #{:o})) win-type))]
                               (if matches
                                 (first matches)
                                 nil)))
        win-situations-with-ties (map has-win-situation? win-types)
        win-situations-no-ties (filter #(not (nil? %)) win-situations-with-ties)]
    (first win-situations-no-ties)))

(defn get-tic-tac-toe-winner
  [board]
  (let [rows (map set board)
        columns (map set (apply map list board))
        diagonal-top-left (list (set (map #(nth (nth board %) %) (range 3))))
        diagonal-top-right (list (set (map #(nth (nth board %) (- 2 %)) (range 3))))]
    (check-for-win
     rows
     columns
     diagonal-top-left
     diagonal-top-right)))

(assert (= :x (get-tic-tac-toe-winner [[:x :o :x] [:x :o :o] [:x :x :o]])))
(assert (= :o (get-tic-tac-toe-winner [[:o :x :x] [:x :o :x] [:x :o :o]])))
(assert (nil? (get-tic-tac-toe-winner [[:x :o :x] [:x :o :x] [:o :x :o]])))

;; PROBLEM 2

(def reachable-from
  {
   1 #{2 4 5 6 8}
   2 #{1 3 4 5 6 7 9}
   3 #{2 4 5 6 8}
   4 #{1 2 3 5 7 8 9}
   5 #{1 2 3 4 6 7 8 9}
   6 #{1 2 3 5 7 8 9}
   7 #{2 4 5 6 8}
   8 #{1 3 4 5 6 7 9}
   9 #{2 4 5 6 8}
   })

(defn inform-neighbors
  [reachable-map num1 num2]
  (assoc reachable-map
         num1 (conj (get reachable-map num1) num2)
         num2 (conj (get reachable-map num2) num1)))

(defn- update-reachable
  [reachable-map selected-num]
  (cond
    (= selected-num 2) (inform-neighbors reachable-map 1 3)
    (= selected-num 4) (inform-neighbors reachable-map 1 7)
    (= selected-num 6) (inform-neighbors reachable-map 3 9)
    (= selected-num 8) (inform-neighbors reachable-map 7 9)
    (= selected-num 5) (inform-neighbors (inform-neighbors reachable-map 1 9) 3 7)
    :else reachable-map))

(defn valid-path
  [path]
  (loop [reachable-map reachable-from
         seen #{(first path)}
         current (first path)
         accessible-from-current (get reachable-map current)
         remaining-path (rest path)
         next-num (first remaining-path)]
    (if (nil? next-num)
      true
      (if (and (contains? accessible-from-current next-num)
               (not (contains? seen next-num)))
        (let [reachable-map (update-reachable reachable-map current)
              seen (conj seen next-num)
              current next-num
              accessible-from-current (get reachable-map current)
              remaining-path (rest remaining-path)
              next-num (first remaining-path)]
          (recur reachable-map seen current accessible-from-current remaining-path next-num))
        false))))



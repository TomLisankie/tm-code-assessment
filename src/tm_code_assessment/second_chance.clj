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

(def position-type
  {
   1 :corner
   2 :middle-top-bottom
   3 :corner
   4 :middle-left-right
   5 :middle
   6 :middle-left-right
   7 :corner
   8 :middle-top-bottom
   9 :corner
   })
(def accessible-from
  {
   :corner #{2 4 5 6 8}
   :middle-top-bottom #{1 3 4 5 6 7 9}
   :middle-left-right #{1 2 3 5 7 8 9}
   :middle #{1 2 3 4 6 7 8 9}
   })

(defn valid-path
  [path]
  (loop [seen #{(first path)}
         current (first path)
         current-type (get position-type current)
         accessible-from-current (get accessible-from current-type)
         remaining-path (rest path)
         next-num (first remaining-path)]
    (if (nil? next-num)
      true
      (if (and (contains? accessible-from-current next-num)
               (not (contains? seen next-num)))
        (let [seen (conj seen next-num)
              current next-num
              current-type (get position-type current)
              accessible-from-current (get accessible-from current-type)
              remaining-path (rest remaining-path)
              next-num (first remaining-path)]
          (recur seen current current-type accessible-from-current remaining-path next-num))
        false))))



(ns tm-code-assessment.second-chance)

;; PROBLEM 1

(defn- has-win-situation?
  [win-type]
  (let [matches (first (filter #(or (= % #{:x}) (= % #{:o})) win-type))]
    (if matches
      (first matches)
      nil)))

(defn- check-for-win
  [& win-types]
  (let [win-situations-with-ties (map has-win-situation? win-types)
        win-situations-no-ties (filter #(not (nil? %)) win-situations-with-ties)]
    (first win-situations-no-ties)))

(defn get-tic-tac-toe-winner
  [board]
  (let [board-size 3
        rows (map set board)
        columns (map set (apply map list board))
        diagonal-top-left (list (set (map #(nth (nth board %) %)
                                          (range board-size))))
        diagonal-top-right (list (set (map #(nth (nth board %) (- 2 %))
                                           (range board-size))))]
    (check-for-win
     rows
     columns
     diagonal-top-left
     diagonal-top-right)))

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

(def nums-that-affect-reachability
  {2 [[1 3]]
   4 [[1 7]]
   6 [[3 9]]
   8 [[7 9]]
   5 [[1 9] [3 7]]})

(defn- update-neighbor-pair
  [reachable-map num1 num2]
  (assoc reachable-map
         num1 (conj (get reachable-map num1) num2)
         num2 (conj (get reachable-map num2) num1)))

(defn- update-relevant-neighbors
  [reachable-map neighbor-pairs]
  (let [pair (first neighbor-pairs)]
    (if (empty? neighbor-pairs)
      reachable-map
      (update-relevant-neighbors
       (update-neighbor-pair reachable-map (first pair) (second pair))
       (rest neighbor-pairs)))))

(defn- update-reachable
  [reachable-map selected-num]
  (if (some #(= selected-num %) (keys nums-that-affect-reachability))
    (update-relevant-neighbors reachable-map (get nums-that-affect-reachability selected-num))
    reachable-map))

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
      (if (or (not (contains? accessible-from-current next-num))
              (contains? seen next-num))
        false
        (let [reachable-map (update-reachable reachable-map current)
              seen (conj seen next-num)
              current next-num
              accessible-from-current (get reachable-map current)
              remaining-path (rest remaining-path)
              next-num (first remaining-path)]
          (recur reachable-map seen current accessible-from-current remaining-path next-num))))))

;; PROBLEM 3

(defn- get-direction-for-positions
  [start-pos end-pos]
  (let [[x-start y-start] start-pos
        [x-end y-end] end-pos]
    [(- x-end x-start) (- y-end y-start)]))

(defn- get-new-position
  [starting-position direction]
  (let [[x-start y-start] starting-position
        [x-dir y-dir] direction]
    [(+ x-start x-dir) (+ y-start y-dir)]))

(defn- search-in-direction
  [direction char-vec starting-position positions->letters]
  (loop [char-vec (rest char-vec)
         next-pos (get-new-position starting-position direction)
         next-char (get positions->letters next-pos)]
    (if (empty? char-vec)
      1
      (if (not= (first char-vec) next-char)
        0
        (recur
         (rest char-vec)
         (get-new-position next-pos direction)
         (get positions->letters (get-new-position next-pos direction)))))))

(defn- abs
  [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn- is-neighbor
  [pos-letter-pair starting-pos]
  (let [pos (first pos-letter-pair)
        [x y] pos
        [x-start y-start] starting-pos]
    (or (and (= (abs (- x-start x)) 1) (= (- y-start y) 0))
        (and (= (abs (- y-start y)) 1) (= (- x-start x) 0)))))

(defn- get-neighbors
  [starting-position positions->letters]
  (filter #(is-neighbor % starting-position) positions->letters))

(defn- find-word-occurrence
  [starting-position char-vec positions->letters]
  (let [char-vec (rest char-vec)
        neighbors (get-neighbors starting-position positions->letters)
        next-char (first char-vec)
        matches (filter #(= (second %) next-char) neighbors)
        directions-to-search (map
                              #(get-direction-for-positions starting-position (first %))
                              matches)
        search-results (map
                        #(search-in-direction %1 char-vec (first %2) positions->letters)
                        directions-to-search matches)]
    (if (empty? matches)
      0
      (apply + search-results))))

(defn- get-position-to-letters-map
  [matrix]
  (let [pos-letter-pairs (for [y (range (count matrix))]
                           (for [x (range (count (first matrix)))]
                             [[(inc x) (inc y)] (nth (nth matrix y) x)]))]
    (into (hash-map) (reduce into [] pos-letter-pairs))))

(defn- get-positions-for-letter
  [letter positions->letters]
  (map first (filter #(= (second %) letter) positions->letters)))

(defn count-words-in-matrix
  [matrix string]
  (let [char-vec (vec string)
        positions->letters (get-position-to-letters-map matrix)
        starting-positions (get-positions-for-letter (first char-vec) positions->letters)
        occurrences (map #(find-word-occurrence % char-vec positions->letters) starting-positions)]
    (reduce + occurrences)))

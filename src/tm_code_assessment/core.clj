(ns tm-code-assessment.core
  (:require [clojure.set :as set]))

;; 1) CODE REVIEW---------------------------------------------------------------

;; Given code:

(let
    [check (fn [& sets]
             (first ;; take the first of the non-nil results of mapping
              (filter #(not (nil? %)) ;; filter everything that's not nil
                      (map ;; transform all of the sets so that
                       (fn [ss]
                         (let [r (first (filter #(or (= % #{:x}) (= % #{:o})) ss))] ;; filter out sets that contain a single element that's either `:x` or `:o`. Then, give me the first of those
                           (if r (first r) nil))) ;; if there was at least one set that had only one `:x` or `:o` in it, give me whichever symbol it contained.
                       sets))))]
  (defn ttt [board]
    (check
     (map set board) ;; rows
     (map set (apply map list board)) ;; columns
     (list (set (map #(nth (nth board %) %) (range 3)))) ;; diagonal from top left
     (list (set (map #(nth (nth board %) (- 2 %)) (range 3)))) ;; diagonal from top right
   )))

;; What is the code trying to accomplish?
;; It's trying to check a tic-tac-toe board (vector of three vectors, each containing three keywords, `:x` or `:o`) to see which player won.

;; Describe at a high level how it works.
;; The basic idea here is that each set passed into the check function is representing a way one could win at Tic-Tac-Toe. For example, `(map set board)` will return a list of sets each of which contain the elements in each row. The others represent columns, diagonal from top left, and diagonal from top right. If one of these ways of winning is a set of only one element, it means the player represented by that element has full coverage and has won the game.

;; What feedback would you provide in a code review?
;; - More descriptive binding names. Unnecessarily difficult to get an understanding of what the code is doing without descriptive names. Not clear from the start what `r` and `ss` are supposed to represent. Also, what's `check` supposed to be checking? Rename to `check-board` or something else that matches your intention.
;; - Don't use `defn` in a scope that isn't in the top-level. This is bad style, because you're affecting the top-level even though it's happening within a local scope. Whether you TODO
;; - Make sure the board inputted is 3x3 and each row only contains `:x` and `:o` values. Use a `:pre` condition for this as to not clutter the meat of the function.
;; - Although the solution is clever, it's not obvious from the start what exactly you're trying to do. Please write some brief comments in the code to describe your approach at a high level
;; - For the tests, put each row of the board on a separate line. This may be a personal preference thing, but since basically every dev reading it will know how Tic-Tac-Toe is played, it would be helpful to have a direct visual of what each board being tested looks like.
;; - Include tests that check for how the function responds to invalid inputs.
;; - Include a doc-string for the `ttt` function so that future developers can just read that to understand what the function is supposed to do.
;; - Consider renaming `ttt` to `tic-tac-toe-winner` or something similar. If the surrounding code is all Tic-Tac-Toe related, that may not be a problem. If it's not, a future dev may have to spend unnecessary time figuring out what the function is supposed to do, especially without a doc-string for the function.
;; - Consider making `check` a top-level function. This completely depends on surrounding code though. If `check` is exclusively meant to be used for Tic-Tac-Toe, include it in a `let` expression in `ttt`.
;; - Put trailing three parentheses on the previous line. Trailing delimiters should not have their own line in Clojure code


;; (Bonus) How would you write it?
;; My code:

(defn tic-tac-toe-winner
  [board]
  "Evaluates which player (if any) won a game of Tic-Tac-Toe based on a final board"
  {:pre [(seq? board)
         (every? seq? board)
         (= (count board) 3)
         (every? #(= 3 (count %)) board)
         (every? #{:o :x} (flatten board))]}
  (let [check-board (fn [& board-patterns]
                      (first
                       (filter #(not (nil? %))
                               (map
                                (fn [patterns]
                                  (let [pattern (first (filter #(or (= % #{:x}) (= % #{:o})) patterns))]
                                    (if pattern
                                      (first pattern)
                                      nil)))
                                board-patterns))))
        rows (map set board)
        columns (map set (apply map list board))
        diagonal-top-left (list (set (map #(nth (nth board %) %) (range 3))))
        diagonal-top-right (list (set (map #(nth (nth board %) (- 2 %)) (range 3))))]
    (check-board rows columns diagonal-top-left diagonal-top-right)))


;; 2) CODE WRITING 1-------------------------------------------------------------

;; Android has a pattern lock screen with 9 dots:

;; 1 2 3
;; 4 5 6
;; 7 8 9

;; Dots may be connected in any order, but:
;; Each dot may only be used once
;; Dots must be connected with straight lines
;; A dot may not be crossed without being used

;; Write a function in any language that takes a sequence of numbers representing dots to connect and determines if it represents a valid pattern.

(defn valid-passcode-pattern?
  [pattern-seq]
  {:pre [(sequential? pattern-seq)
         (<= 2 (count pattern-seq))
         (every? int? pattern-seq)]}
  (let [dot-grid [[1 2 3]
                  [4 5 6]
                  [7 8 9]]
        dot-id->pos (into {} (for [x (range (count (first dot-grid)))
                                   y (range (count dot-grid))]
                               [(nth (nth dot-grid y) x) [x y]]))
        direct-neighbors-of-pos (fn [pos]
                                  (let [x (first pos)
                                        y (second pos)]
                                    #{[(+ x 1) y]
                                      [(- x 1) y]
                                      [x (+ y 1)]
                                      [x (- y 1)]}))
        surrounding-positions (fn [dot-id used-dot-ids used-positions]
                                (if (or (nil? (get dot-id->pos dot-id)) (contains? used-dot-ids dot-id)) ;; need to make sure the dot exists and hasn't already been seen
                                  false
                                  (let [dot-position (get dot-id->pos dot-id)
                                        x (first dot-position)
                                        y (second dot-position)
                                        reachable-positions #{[(- x 1) y] ;; these positions include dots above, below, left, right, diagonal, and knight's jumps away for a given position
                                                              [(- x 1) (- y 1)]
                                                              [x (- y 1)]
                                                              [(+ x 1) (- y 1)]
                                                              [(+ x 1) y]
                                                              [(+ x 1) (+ y 1)]
                                                              [x (+ y 1)]
                                                              [(- x 1) (+ y 1)]
                                                              [(- x 1) (+ y 2)]
                                                              [(+ x 1) (+ y 2)]
                                                              [(- x 1) (- y 2)]
                                                              [(+ x 1) (- y 2)]
                                                              [(+ x 2) (- y 1)]
                                                              [(- x 2) (- y 1)]
                                                              [(+ x 2) (+ y 1)]
                                                              [(- x 2) (+ y 1)]}]
                                    (set/union reachable-positions (apply set/union (map
                                                                                     #(when (contains? used-positions %)
                                                                                        (direct-neighbors-of-pos %))
                                                                                     reachable-positions))))))]
    (loop [rest-of-pattern-seq pattern-seq
           current-dot-id (first rest-of-pattern-seq)
           dot-to-connect-to (second rest-of-pattern-seq)
           seen-dot-ids #{}
           seen-positions #{}
           reachable-from-current-dot (surrounding-positions current-dot-id seen-dot-ids seen-positions)]

      (if (or (false? reachable-from-current-dot) (contains? seen-dot-ids current-dot-id)) ;; current-dot-id is not valid or it has been used already
        false
        (if (nil? dot-to-connect-to)
          true
          (if (contains? reachable-from-current-dot (get dot-id->pos dot-to-connect-to)) ;; is the dot you're trying to connect to reachable from the current dot?
            (recur (rest rest-of-pattern-seq)
                   dot-to-connect-to
                   (second (rest rest-of-pattern-seq))
                   (conj seen-dot-ids current-dot-id)
                   (conj seen-positions (get dot-id->pos current-dot-id))
                   (surrounding-positions dot-to-connect-to (conj seen-dot-ids current-dot-id) (conj seen-positions (get dot-id->pos current-dot-id))))
            false))))))

;; Bonus
;; - Given a PIN entered with a 9-digit keypad instead, how many digits would be required to have more possible combinations than the pattern lock?
;; - If the grid can be extended to any square size (ex 4x4), but the pattern can only have up to 8 dots, what’s the smallest size at which there would be as many possibilities as an 8 character alphanumeric (a-z, A-Z, 0-9) password?





;; 3) CODE WRITING 2-------------------------------------------------------------

;; Write a function in any language that takes a matrix of characters and a word, and returns the number of times that word appears in the matrix. The word may appear left-to-right, right-to-left, top-to-bottom, or bottom-to-top.
;; Example:
;; A O T D L R O W
;; L C B M U M L U
;; D R U J D B L J
;; P A Z H Z Z E F
;; B C Z E L F H W
;; R K U L V P P G
;; A L B L P O P Q
;; B E M O P P J Y

(defn occurrences-of-word-in-grid
  [grid word]
  {:pre [(sequential? grid)
         (every? sequential? grid)
         (every? #(= (count (first grid)) (count %)) grid)
         (every? char? (flatten grid))]}
  (let [char-grid->all-pos-map
        (fn [grid]
          (let [char-pos-pairs (for [x (range (count (first grid)))
                                     y (range (count grid))]
                                 [(nth (nth grid y) x) [x y]])
                all-letters (set (map first char-pos-pairs))
                char->all-pos-pairs (map (fn [letter]
                                           (vec [letter
                                                 (set
                                                  (map
                                                   second
                                                   (filter
                                                    #(= (first %) letter)
                                                    char-pos-pairs)))]))
                                         all-letters)]
            (into {} char->all-pos-pairs)))
        char->all-pos (char-grid->all-pos-map grid)
        possible-matches (fn [pos word-length]
                           (let [x (first pos)
                                 y (second pos)
                                 nums (range 1 word-length)
                                 above (map #(vec [x (+ y %)]) nums)
                                 below (map #(vec [x (- y %)]) nums)
                                 left (map #(vec [(- x %) y]) nums)
                                 right (map #(vec [(+ x %) y]) nums)]
                             [above below left right]))
        match-patterns (map #(possible-matches % (count word)) (get char->all-pos (first word)))
        match? (fn [word positions]
                 (loop [word-remainder (rest word)
                        remaining-positions positions]
                   (if (empty? word-remainder)
                     1
                     (if (contains?
                          (get char->all-pos (first word-remainder))
                          (first remaining-positions))
                       (recur (rest word-remainder)
                              (rest remaining-positions))
                       0))))
        matches (loop [the-patterns match-patterns
                       sums []]
                  (if (empty? the-patterns)
                    sums
                    (recur (rest the-patterns)
                           (concat sums (map #(match? word %) (first the-patterns))))))]
    (apply + matches)))

;; 4) BONUS QUESTION-------------------------------------------------------------

;; Develop a Clojure algorithm or theory on how to find the number of triangles that can be made inside of an equilateral triangle where a, b, and c are the number of internal lines coming from the corresponding point and no 3 of those lines cross at a single point
;; https://bit.ly/2yHzeYp




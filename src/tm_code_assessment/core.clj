(ns tm-code-assessment.core)

;; 1) CODE REVIEW---------------------------------------------------------------

;; Given code:
(let
    [check (fn [& sets]
             (first (filter #(not (nil? %))
                            (map
                             (fn [ss]
                               (let [r (first (filter #(or (= % #{:x}) (= % #{:o})) ss))]
                                 (if r (first r) nil)))
                             sets))))]
  (defn ttt [board]
    (check
     (map set board)
     (map set (apply map list board))
     (list (set (map #(nth (nth board %) %) (range 3))))
     (list (set (map #(nth (nth board %) (- 2 %)) (range 3))))
     )))

;; What is the code trying to accomplish?
;; It's supposed to be checking a tic-tac-toe board (vector of three vectors, each containing three keywords, `:x` or `:o`) to see which player won.

;; Describe at a high level how it works.
;; The basic idea here is that each set passed into the check function is representing a way one could win at Tic-Tac-Toe. For example, `(map set board)` will return a list of sets each of which contain the elements in each row. The others represent columns, diagonal from top left, and diagonal from top right. If one of these ways of winning is a set of only one element, it means the player represented by that element has full coverage and has won the game.

;; What feedback would you provide in a code review?
;; - For the tests, put each row of the board on a separate line. This may be a personal preference thing, but since basically every dev reading it will know how Tic-Tac-Toe is played, it would be helpful to have a direct visual of what each board being tested looks like.
;; - More descriptive binding names. More difficult to get an understanding of what the code is doing without descriptive names. Questions a reader of the code might ask: `check`? Check what? What's `ss`? What's that supposed to be? `r`? Oh, 'row' maybe?
;; - Don't use `defn` in a scope that isn't in the top-level. This is bad style, because you're affecting the top-level even though it happens in a level below the top. TODO: elaborate.
;; - Make sure the board inputted is 3x3. Use a `:pre` condition for this as to not clutter the meat of the function.

;; (Bonus) How would you write it?
;; My code:
(let ;; USE A PRE-CONDITION to check for proper size.
    [check (fn [& sets]
             (first ;; take the first of the non-nil results of mapping
              (filter #(not (nil? %)) ;; filter everything that's not nil
                            (map ;; transform all of the sets so that
                             (fn [ss]
                               (let [r (first (filter #(or (= % #{:x}) (= % #{:o})) ss))] ;; filter out sets that contain a single element that's either `:x` or `:o`. Then, give me the first of those
                                 (if r (first r) nil))) ;; if there was at least one set that had only one `:x` or `:o` in it, give me whichever symbol it contained.
                             sets))))]
  (defn ttt [board]
    {:pre [(= (count board) 3)
           (= 3 (count (filter #(= 3 (count %)) board)))]}
    (check
     (map set board) ;; rows
     (map set (apply map list board)) ;; columns
     (list (set (map #(nth (nth board %) %) (range 3)))) ;; diagonal from top left
     (list (set (map #(nth (nth board %) (- 2 %)) (range 3))))))) ;; diagonal from top right


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


;; 4) BONUS QUESTION-------------------------------------------------------------

;; Develop a Clojure algorithm or theory on how to find the number of triangles that can be made inside of an equilateral triangle where a, b, and c are the number of internal lines coming from the corresponding point and no 3 of those lines cross at a single point
;; https://bit.ly/2yHzeYp




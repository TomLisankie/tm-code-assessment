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
;;

;; What feedback would you provide in a code review?
;; For the tests, put each row of the board on a separate line. This may be a personal preference thing, but since basically every dev reading it will know how Tic-Tac-Toe is played, it would be helpful to have a direct visual of what each board looks like.

;; (Bonus) How would you write it?
;; My code:


;; 2) CODE WRITING 1-------------------------------------------------------------




;; 3) CODE WRITING 2-------------------------------------------------------------




;; 4) BONUS QUESTION-------------------------------------------------------------


(ns day04-bingo.core
  (:require [clojure.string :as str]))

(defn construct-board [board-string]
  (when (not= (first board-string) "")
    (let [splits (->> board-string
                      (map #(str/split % #" ")))]
      (for [b splits
            :let [cleaned (remove #(= % "") b)]]
        (into [] (map #(Integer/parseInt %) cleaned))))))

(defn parse-input [input]
  (let [[num-seq _ & boards] (->> input
                                  (str/split-lines)
                                  (partition-by #(= "" %)))
        nums (map #(Integer/parseInt %)
                  (-> (first num-seq)
                      (str/split #",")))
        bs (remove nil? (map construct-board boards))]
    [nums bs]))

(defn check-horizontals [board]
  (when (seq board)
    (if (apply = (first board))
      true
      (recur (rest board)))))

(defn check-verticals [board]
  (when (seq (first board))
    (let [col (map first board)]
       (if (apply = col)
         true
         (recur (map rest board))))))

(defn get-diagonal [board]
  (loop [b board
         c 0
         d1 []]
    (if (= c 5) d1
        (recur (map rest (rest b))
         (inc c)
         (conj d1 (first (first b)))))))

(defn check-diagonals [board]
  (let [d1 (get-diagonal board)
        d2 (get-diagonal (map reverse board))]
    (or
     (apply = d1)
     (apply = d2))))

(def test-board '([1 3 "x" 7 "x"]
                  ["x" "x" "x" "x" "x"]
                  [5 6 "x" 8 7]
                  [2 "x" "x" 7 9]
                  ["x" 7 "x" 5 6]))

(defn check-win [board]
  (or
   (check-horizontals board)
   (check-verticals   board)))
   ;; (check-diagonals   board))) ;just read that diagonals don't count...

(defn mark-line [line number]
  (into [] (map #(if (= number %) "x" %) line)))

(defn mark-board [board number]
  (map #(mark-line % number) board))

(defn winner-score [board number]
  (let [clean (remove #(= % "x") (flatten board))]
    (* number (apply + clean))))

(defn call-numbers [input]
  (let [[nums boards] (parse-input input)]
    (loop [[n & ns] nums
           b boards]
      (if n
        (let [updated (map #(mark-board % n) b)
              winner (for [i updated
                           :when (check-win i)]
                       i)]
          (if (seq winner) (winner-score winner n)
              (recur ns updated)))
        "no winner"))))

(comment
  (println (call-numbers (slurp "input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; PART 2
;;

(defn call-numbers-for-last-board [input]
  (let [[nums boards] (parse-input input)]
    (loop [[n & ns] nums
           b boards]
      (if n
        (let [marked (map #(mark-board % n) b)
              wins (for [i marked
                         :when (check-win i)]
                     i)
              updated (loop [m marked
                             w wins]
                        (if (empty? w) m
                            (recur (remove #{(first w)} m)
                                   (rest w))))]
          (if (empty? updated) (winner-score wins n)
              (recur ns updated)))
        "No winner"))))

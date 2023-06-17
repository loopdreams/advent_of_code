(ns day2-rps.core
  (:require [clojure.string :as s]))

(def codes {:A [1  "rock"]
            :B [2 "paper"]
            :C [3 "scissors"]
            :X [1  "rock"]
            :Y [2 "paper"]
            :Z [3 "scissors"]})

(defn strategy-guide [strategy]
  (let [s (case strategy
            :default "input.txt"
            :alternative (do (alternative-strategy 20) "s2.txt")
            :random (do (random-strategy 20) "s3.txt"))]
    (map #(let [x (first %) y (last %)] [(str x) (str y)])
         (s/split-lines (slurp s)))))

(defn score [outcome p1 p2]
  (case outcome
    :p1 [(+ p1 6) p2]
    :p2 [p1 (+ p2 6)]
    :draw [(+ p1 3) (+ p2 3)]))

(defn single-round [player1 player2]
  (let [p1 (first ((keyword player1) codes))
        p2 (first ((keyword player2) codes))]
    (cond
      (= p1 p2)               (score :draw p1 p2)
      (and (= p1 1) (= p2 3)) (score :p1 p1 p2)
      (and (= p2 1) (= p1 3)) (score :p2 p1 p2)
      (> p1 p2)               (score :p1 p1 p2)
      :else                   (score :p2 p1 p2))))

(defn play-rounds [strategy]
  (loop [rounds strategy
         p1 0
         p2 0]
    (if (empty? rounds) [p1 p2]
        (let [[r1 r2] (first rounds)
              [val1 val2] (single-round r1 r2)]
          (recur
           (rest rounds)
           (+ p1 val1)
           (+ p2 val2))))))

(defn -main [& args]
  (let [[p1 p2] (play-rounds  (strategy-guide :default))]
    (cond
      (> p1 p2) (printf "Player 1 wins, with a score of %d to %d" p1 p2)
      (> p2 p1) (printf "Player 2 wins, with a score of %d to %d" p2 p1)
      :else (printf "It was a tie, both players score %d." p1))
    p2))


;;; PART 2
(defn determine-real-turn [p1-turn p2-turn]
  (case p2-turn
    :X (if (= p1-turn 1) 3 (- p1-turn 1))
    :Y p1-turn
    :Z (if (= p1-turn 3) 1 (+ 1 p1-turn))))

(defn single-round-adjusted [player1 player2]
  (let [p1 (first ((keyword player1) codes))
        p2 (determine-real-turn p1 (keyword player2))]
    (cond
      (= p1 p2)               (score :draw p1 p2)
      (and (= p1 1) (= p2 3)) (score :p1 p1 p2)
      (and (= p2 1) (= p1 3)) (score :p2 p1 p2)
      (> p1 p2)               (score :p1 p1 p2)
      :else                   (score :p2 p1 p2))))

(defn play-rounds-adjusted [strategy]
  (loop [rounds strategy
         points 0]
    (if (empty? rounds) points
        (let [[r1 r2] (first rounds)
              [val1 val2] (single-round-adjusted r1 r2)]
          (recur
           (rest rounds)
           (+ points val2))))))

(defn part2 []
  (play-rounds-adjusted (strategy-guide :default)))

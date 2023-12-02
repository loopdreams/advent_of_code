(ns day02-cube-conundrum.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-color-vals [s colour]
  (mapv (comp parse-long second)
        (re-seq (re-pattern (str #"(\d+) " colour)) s)))

(defn parse-line [line]
  (let [[game balls] (str/split line #": ")]
    [(parse-long (re-find #"\d+" game))
     {:blue  (parse-color-vals balls "blue")
      :green (parse-color-vals balls "green")
      :red   (parse-color-vals balls "red")}]))

(defn parse-input [input]
  (into (sorted-map)
        (for [line (str/split-lines input)]
          (parse-line line))))

;; Part1 Conditions
;; 12 red
;; 13 green
;; 14 blue

(defn check-vals-part-1 [vals]
  (and (every? #(<= % 12) (:red vals))
       (every? #(<= % 13) (:green vals))
       (every? #(<= % 14) (:blue vals))))

(defn sum-part-1-coditions [input]
  (let [games (parse-input input)]
    (reduce (fn [sum [game-no vals]]
              (if (check-vals-part-1 vals)
                (+ sum game-no)
                sum))
            0
            games)))


(comment
  (sum-part-1-coditions input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn minimum-set-power [vals]
  (reduce *
          (for [color (keys vals)]
            (apply max (color vals)))))

(defn sum-of-minimum-set-powers [input]
  (let [games (parse-input input)]
    (reduce +
            (for [game (keys games)]
              (minimum-set-power (get games game))))))

(comment
  (sum-of-minimum-set-powers input))

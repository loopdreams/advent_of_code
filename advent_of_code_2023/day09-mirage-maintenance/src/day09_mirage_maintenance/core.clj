(ns day09-mirage-maintenance.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (map parse-long (str/split line #" "))))

(defn diffs [s]
  (->> s
       (map #(repeat 2 %))
       flatten
       rest
       drop-last
       (partition 2)
       (map #(- (apply - %)))))

(defn calculate-next-step [sequence]
  (loop [s sequence
         d (diffs s)
         lasts []]
    (if (every? #(= % 0) d) (+ ( last sequence ) (reduce + lasts))
        (recur d
               (diffs d)
               (conj lasts (last d))))))

(defn part-1 [input]
  (for [sequence (parse-input input)]
    (calculate-next-step sequence)))

(comment
  (println
   (->> (part-1 input)
        (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn part-2 [input]
  (for [sequence (map reverse (parse-input input))]
    (calculate-next-step sequence)))

(comment
  (->> (part-2 input)
       (reduce +)))

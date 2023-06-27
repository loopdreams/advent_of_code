(ns day01-report-repair.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (->> input str/split-lines (map #(Integer/parseInt %))))



(defn part1 [nums]
  (reduce * (for [n nums
                  :let [sums (map #(+ n %) (remove #{n} nums))]
                  :when ((set sums) 2020)]
              n)))

(comment
  (part1 (parse-input input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(defn part2 [nums]
  (->> nums
       (map
        (fn [val]
          (let [pair-vals (map #(+ val %) (remove #{val} nums))
                three-vals (for [x pair-vals
                                 y (remove #{val} nums)]
                             (+ x y))]
            (when ((set three-vals) 2020)
              val))))
       (remove nil?)
       (reduce *)))

(comment
  (part2 (parse-input input)))

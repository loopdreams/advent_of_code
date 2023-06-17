(ns day07-whale.core
  (:require [clojure.string :as str]))

(def input (slurp "sample.txt"))
(def actual-input (slurp "input.txt"))

(defn parse-input [input]
  (map #(Integer/parseInt %)
       (-> input
           (str/split-lines)
           first
           (str/split #","))))

(defn re-position [input]
  (let [starts (parse-input input)
        possible (range 0 (-> starts sort reverse first inc))]
    (into {} (for [i possible
                   :let [distances (map #(abs (- % i)) starts)]]
               [i (apply + distances)]))))

(defn best-position [input]
  (let [positions (re-position input)]
    (second (first (sort-by second positions)))))

(comment
  (best-position actual-input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
;;

(defn move-cost [start end]
  (let [distance (abs (- start end))]
    (apply + (range 0 (inc distance)))))


(defn part2-costs [input]
  (let [starts (parse-input input)
        possible (range 0 (-> starts sort reverse first inc))]
    (into {} (for [i possible
                   :let [distances (map #(move-cost % i) starts)]]
               [i (apply + distances)]))))

(defn part-2-best [input]
  (let [[[_ cost] & _] (->> input
                            (part2-costs)
                            (sort-by second))]
    cost))

(comment
  (part-2-best input))

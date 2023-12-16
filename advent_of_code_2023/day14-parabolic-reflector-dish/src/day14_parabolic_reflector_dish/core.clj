(ns day14-parabolic-reflector-dish.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn input->cols [input]
  (loop [lines (str/split-lines input)
         result []]
    (if (empty? (first lines)) result
        (recur (map rest lines)
               (conj result (map first lines))))))

(defn sort-col-part [part]
  (if (some #{\O} part)
    (reverse (sort part))
    part))

(defn sort-col-north [col]
  (->> (partition-by #(= % \#) col)
       (map sort-col-part)
       flatten))

(defn part-1 [input]
  (let [sorted (map #(map-indexed vector %)
                    (->> (input->cols input)
                         (map sort-col-north)
                         (map reverse)))]
    (reduce (fn [acc col]
              (let [col-vals (for [entry col
                                   :let [[idx v] entry]
                                   :when (= v \O)]
                               (inc idx))]
                (+ acc (apply + col-vals))))
            0
            sorted)))

(comment
  (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn input->rows [input]
  (str/split-lines input))

(defn rows<->cols [rows]
  (loop [r rows
         result []]
    (if (empty? (first r)) result
        (recur (map rest r)
               (conj result (map first r))))))

(defn sort-part [direction part]
  (if (some #{\O} part)
    (case direction
      :left (reverse (sort part))
      :right (sort part))
    part))

(defn sort-row-col [direction col]
  (->> (partition-by #(= % \#) col)
       (map #(sort-part direction %))
       flatten))

(defn calculate-load [rows]
  (let [stones (reverse
                (map #(count (filter #{\O} %)) rows))]
    (apply +
           (for [r (map-indexed vector stones)]
             (* (inc (first r)) (second r))))))

(defn cycle-dish [rows]
  (reduce (fn [rs dir]
            (map #(sort-row-col dir %) (rows<->cols rs)))
          rows
          [:left :left :right :right]))

(defn sample-sequence [rows start end]
  (loop [rs (nth (iterate cycle-dish rows) start)
         step start
         results []]
    (if (= step end) results
        (let [new-rs (cycle-dish rs)]
          (recur
           new-rs
           (inc step)
           (conj results (calculate-load rs)))))))

(defn detect-seq [sequence]
  (loop [window 2]
    (when (< window (/ (count sequence) 2))
      (let [a (take window sequence)
            b (take window (drop window sequence))]
        (if (= a b)
          a
          (recur (inc window)))))))

(defn value-at [sequence start-idx target]
  (nth sequence (mod (- target start-idx) (count sequence))))

(defn part-2 [input check-start check-end target]
  (value-at (detect-seq
             (sample-sequence (str/split-lines input)
                              check-start
                              check-end))
            check-start
            target))

(comment
  ;; the 'start'/'end' parts to check are just guesses...
  (part-2 sample 100 120 1000000000)
  (part-2 input 500 600 1000000000))



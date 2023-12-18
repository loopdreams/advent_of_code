(ns day15-lens-library.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input  (slurp  "input.txt"))

(defn parse-input [input]
  (str/split (str/replace input #"\n" "") #","))

(defn compute-hash [str]
  (reduce (fn [acc char]
            (mod (* 17 (+ acc (int char)))
               256))
          0
          str))

(comment
  ;; Part 1
  (reduce + (map compute-hash (parse-input input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn update-add-array [array [new-k _ :as new]]
  (let [keys (map first array)]
    (if (some #{new-k} keys)
      (assoc array
             (ffirst (filter #(= (first (second %)) new-k)
                             (map-indexed vector array)))
             new)
      (conj (if array array []) new))))

(defn update-remove-array [array target]
  (if (some #{target} (map first array))
    (into [] (remove #{(first (filter #(= (first %) target) array))} array))
    array))


(defn fill-boxes [instructions]
  (reduce (fn [m instruction]
            (let [[_ label _ num] (re-find #"(\w+)(=|-)(\d+)?" instruction)
                  box (compute-hash label)
                  num (when num (parse-long num))]
              (if num
                (update m box #(update-add-array % [label num]))
                (update m box #(update-remove-array % label)))))
          {}
          instructions))

(defn focusing-power [input]
  (let [boxes (fill-boxes (parse-input input))]
    (reduce (fn [acc k]
              (let [vals (map-indexed vector (boxes k))
                    b-val (inc k)]
                (+ acc
                   (reduce +
                           (for [v vals
                                 :let [[idx [_ len]] v]]
                             (* b-val (inc idx) len))))))
            0
            (keys boxes))))

(comment
  (focusing-power input))

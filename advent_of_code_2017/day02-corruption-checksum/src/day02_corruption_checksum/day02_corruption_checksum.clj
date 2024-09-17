(ns day02-corruption-checksum.day02-corruption-checksum
  (:require
   [clojure.string :as str]))

(def input (slurp "D02.txt"))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (map parse-long (re-seq #"\d+" line))))


(defn checksum [ns]
  (- (apply max ns) (apply min ns)))

(defn part-1 [input]
  (let [lines (parse-input input)]
    (reduce (fn [acc line]
              (+ acc (checksum line)))
            0
            lines)))

(part-1 input)

;; Part 2

(defn evenly-divide [x y]
  (or (zero? (mod x y))
      (zero? (mod y x))))

(defn evenly-divide-cands [cand lst]
  (loop [[el & els] lst]
    (if-not el nil
            (if (evenly-divide cand el)
              el
              (recur els)))))

(defn evenly-divide-line [line]
  (loop [[n & ns] line]
    (if-not n nil
            (let [cand (evenly-divide-cands n (remove #{n} line))]
              (if cand
                (let [[low high] (sort [n cand])]
                  (/ high low))
                (recur ns))))))

(defn part-2 [input]
  (let [lines (parse-input input)]
    (reduce (fn [acc line]
              (+ acc (evenly-divide-line line)))
            0
            lines)))

(part-2 input)

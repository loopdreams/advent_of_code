(ns day13-packet-scanners.core
  (:require
   [clojure.string :as str]))

(def input (slurp "D13.txt"))

(def sample (slurp "sample.txt"))

(defn parse-input [input]
  (reduce (fn [res line]
            (let [scanner (map parse-long (str/split line #": "))]
              (conj res scanner)))
          []
          (str/split-lines input)))

(def make-range
  (memoize
   (fn [r]
     (concat (range r) (reverse (range 1 (dec r)))))))

(defn scanner-pos [idx r]
  (let [ran (make-range r)
        m (count ran)]
    (nth ran (mod idx m))))

(defn search-caught [s-ranges offset]
  (filter (fn [[idx range]] (zero? (scanner-pos (+ idx offset) range))) s-ranges))

(defn sum-caught [caughts]
  (reduce #(+ %1 (apply * %2)) 0 caughts))

(defn part-1 [input]
  (-> input
      parse-input
      (search-caught 0)
      sum-caught))

(comment
  (part-1 input))

;; Part 2

(defn find-path [s-ranges]
  (loop [n 0]
    (if (empty? (search-caught s-ranges n))
      n
      (recur (inc n)))))

(comment
  (time
   (println
    (find-path (parse-input input)))))


;; Final Time
;; "Elapsed time: 53955.329375 msecs"

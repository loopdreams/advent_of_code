(ns day3-rucksack.core
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def rucksacks (s/split-lines (slurp "input.txt")))

(defn ascii->keywords [list]
  (->> list
       (map char)
       (map str)
       (map keyword)))

(def priorities
  (let [uppers (ascii->keywords (range 65 91))
        lowers (ascii->keywords (range 97 123))
        uvals (range 27 53)
        lvals (range 1 27)]
    (apply assoc {}
           (concat
            (interleave lowers lvals)
            (interleave uppers uvals)))))

(defn sort-rucksack [string]
  (let [[part-a part-b] (partition (/ (count string) 2) string)]
    (loop [x part-a
           result []]
      (if (empty? result)
        (recur
         (rest x)
         (if (some #(= (first x) %) part-b)
           (conj result (first x))
           result))
        (first result)))))

(defn sort-all [rucksacks]
  (loop [rs rucksacks
         dupes []
         count 1]
    (if (empty? rs) dupes
        (recur
         (rest rs)
         (conj dupes [count (sort-rucksack (first rs))])
         (inc count)))))

(defn sum-of-priorities [items]
  (let [keys (map keyword (map str items))]
    (apply + (map #(% priorities) keys))))


(defn -main
  [& args]
  (let [sorted (sort-all rucksacks)
        priorities (sum-of-priorities (map second sorted))]
    (pp/pprint [:missing-items sorted :sum-of-priorities priorities])))

;; Part 2
;;
(defn find-common-items [rs1 rs2]
  (loop [x rs1
         result []]
    (if (empty? x) (s/join (map str (set result)))
      (recur
       (rest x)
       (if (some #(= (first x) %) rs2)
         (conj result (first x))
         result)))))

(defn sort-badges [rucksacks]
  (loop [[a b c & rest] rucksacks
         badges []]
    (if c
      (recur
       rest
       (conj badges (find-common-items c (find-common-items a b))))
      badges)))

(defn part2-badges []
  (let [badges (sort-badges rucksacks)]
    (sum-of-priorities badges)))


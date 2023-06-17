(ns day4-cleanup.core
  (:require [clojure.string :as s]
            [clojure.set :refer [subset?]]))

(def rota (map #(s/split % #",")
               (s/split-lines (slurp "input.txt"))))

(defn ch->int [s]
  (-> s
      (str)
      (Integer/parseInt)))

(defn get-nums [str-range]
  (->> str-range
       (re-find #"(\d+)-(\d+)")
       (rest)
       (map ch->int)))

(defn expand-single [[elf1 elf2]]
  (let [[x y] (get-nums elf1)
        [a b] (get-nums elf2)]
    [(range x (inc y)) (range a (inc b))]))

(defn compare-rooms [[elf1 elf2]]
  (let [x (set elf1)
        y (set elf2)]
    (or (subset? x y)
        (subset? y x))))

(defn -main
  [& args]
  (let [trans-rota (map expand-single rota)
        overlapping-rooms (->> trans-rota
                               (map compare-rooms)
                               (filter true?)
                               (count))]
    (printf "There are %d pairs where the range fully contains the other." overlapping-rooms)
    overlapping-rooms))

(defn part-two-all-overlaps [[elf1 elf2]]
  (let [x (set elf1)
        y (set elf2)
        over (clojure.set/intersection x y)]
    over))

(defn part-two []
  (let [rs (map expand-single rota)
        overs (map part-two-all-overlaps rs)]
    (count (remove empty? overs))))


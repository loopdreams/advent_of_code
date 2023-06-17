(ns day06-lanternfish.core
  (:require [clojure.string :as str]))

(def input (slurp "sample.txt"))

(defn parse-input [input]
  (map #(Integer/parseInt %)
       (-> input
           (str/split-lines)
           first
           (str/split #","))))

(defn update-fish [fish]
  (let [normal (set (range 1 9))]
    (if (normal fish) [(dec fish)] [6 8])))

(defn update-all [fishes]
  (flatten (map update-fish fishes)))

(defn counting-fish [input days]
  (let [fish (atom (parse-input input))]
    (dotimes [n days]
      (swap! fish update-all))
    (count @fish)))

(comment
  (time (counting-fish (slurp "input.txt") 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
;; A more efficient counting...
;;

(def fish-init {:zeros 0
                :ones 0
                :twos 0
                :threes 0
                :fours 0
                :fives 0
                :sixes 0
                :sevens 0
                :eights 0})

(defn map-input [input fish-init]
  (let [nums (parse-input input)
        fish (atom fish-init)]
    (last (for [i nums
                :let [x (case i
                          0 :zeros
                          1 :ones
                          2 :twos
                          3 :threes
                          4 :fours
                          5 :fives
                          6 :sixes)]]
            (swap! fish update-in [x] inc)))))

(def fish-matches {:ones   :zeros
                   :twos   :ones
                   :threes :twos
                   :fours  :threes
                   :fives  :fours
                   :sixes  :fives
                   :sevens :sixes
                   :eights :sevens})

(defn update-key [map key n]
    (if (= key :zeros)
      (-> map
          (update :zeros - n)
          (update :sixes + n)
          (update :eights + n))
      (let [match (key fish-matches)]
        (-> map
            (update key - n)
            (update match + n)))))

(defn fish-update-part-2 [map]
  (let [keys (keys (remove #(-> % val (= 0)) map))]
    (loop [[k & ks] keys
           m map]
      (if k
        (recur ks
               (update-key m k (k map)))
        m))))

(defn counting-fish-part-2 [input days]
  (let [fish (atom (map-input input fish-init))]
    (dotimes [n days]
      (swap! fish fish-update-part-2))
    (apply + (vals @fish))))

(comment
  (println (counting-fish-part-2 (slurp "input.txt") 256)))

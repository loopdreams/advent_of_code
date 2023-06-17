(ns day06-tuning.core
  (:gen-class)
  (:require [clojure.string :as s]))

(def datastream "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(def input (slurp "input.txt"))

(defn find-marker [data]
  (loop [dat (seq data)
         count 4]
    (let [[a b c d] dat]
      (if (distinct? a b c d) count
          (recur
           (rest dat)
           (inc count))))))

(defn find-message-marker [data]
  (loop [dat (seq data)
         count 14]
    (let [[a b c d e f g h i j k l m n] dat]
      (if (distinct? a b c d e f g h i j k l m n) count
          (recur
           (rest dat)
           (inc count))))))

(defn -main
  [& _]
  (let [result (find-marker input)]
    (println result)
    result))

(defn part2 []
  (let [result (find-message-marker input)]
    (println result)
    result))

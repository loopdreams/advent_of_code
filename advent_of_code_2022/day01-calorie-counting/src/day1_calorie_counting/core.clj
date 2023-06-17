(ns day1-calorie-counting.core
  (:require [clojure.string :as s]))

(def calorie-list (s/split-lines (slurp "input.txt")))

(defn calories [elf]
  (reduce + (map #(Integer/parseInt %) elf)))
  
(defn most-calories [calorie-list tops]
  (let [elves (->> calorie-list
                   (partition-by #(= % ""))
                   (remove #(= (first %) "")))]
    (loop [c 1
           es elves
           result []]
      (if (not= c (inc (count elves)))
        (recur
         (inc c)
         (rest es)
         (conj result [c (calories (first es))]))
        (take tops
         (reverse
          (sort-by second result)))))))

(defn -main
  [& args]
  (let [[elf calories] (most-calories calorie-list 1)]
    (printf "The elf carrying the most calories is elf number %d.\n They are carrying %d calories." elf calories)
    calories))

(defn part-2
  [& _]
  (let [elfs (map second (most-calories calorie-list 3))]
    (reduce + elfs)))

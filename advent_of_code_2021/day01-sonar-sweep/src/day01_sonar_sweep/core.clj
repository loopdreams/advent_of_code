(ns day01-sonar-sweep.core
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(Integer/parseInt %))))

(defn depth-increases
  ([depths] (depth-increases depths 0))
  ([depths count]
   (let [[x y & rest] depths]
     (if y
       (recur (cons y rest)
              (if (< x y)
                (inc count)
                count))
       count))))

(defn part1 [input]
  (let [depths (parse-input input)]
    (depth-increases depths)))

(comment (part1 (slurp "input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(defn grouped-depth-increases
  ([depths] (grouped-depth-increases depths 0))
  ([depths count]
   (let [[a b c d & rest] depths]
     (if d
       (let [group1 (+ a b c)
             group2 (+ b c d)]
         (recur (concat [b c d] rest)
                (if (< group1 group2)
                  (inc count)
                  count)))
       count))))

(defn part2 [input]
  (let [depths (parse-input input)]
    (grouped-depth-increases depths)))

(comment
  (part2 (slurp "input.txt")))

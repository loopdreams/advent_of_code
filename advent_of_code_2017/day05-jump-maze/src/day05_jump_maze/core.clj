(ns day05-jump-maze.core
  (:require
   [clojure.string :as str]))

(def sample [0 3 0 1 -3])

(def input (slurp "D05.txt"))

(defn find-exit [ns]
  (let [exit-pos (count ns)]
    (loop [pos 0
           instructions ns
           jumps 0]
      (if (>= pos exit-pos) jumps
          (let [active-val (nth instructions pos)]
            (recur (+ pos active-val)
                   (assoc instructions pos (inc active-val))
                   (inc jumps)))))))

(comment
  (->> input
       (str/split-lines)
       (mapv parse-long)
       find-exit))

;; Part 2

(defn find-exit-v2 [ns]
  (let [exit-pos (count ns)]
    (loop [pos 0
           instructions ns
           jumps 0]
      (if (>= pos exit-pos) jumps
          (let [active-val (nth instructions pos)]
            (recur (+ pos active-val)
                   (assoc instructions pos (if (>= active-val 3)
                                             (dec active-val)
                                             (inc active-val)))
                   (inc jumps)))))))


(comment
  (->> input
       (str/split-lines)
       (mapv parse-long)
       find-exit-v2))

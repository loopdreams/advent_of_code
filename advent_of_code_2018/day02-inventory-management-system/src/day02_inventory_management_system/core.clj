(ns day02-inventory-management-system.core
  (:require
    [clojure.string :as str]))

(def input (str/split-lines (slurp "input.txt")))

(defn count-repeated-letters [s]
  (->> s
       sort
       (partition-by identity)
       (map count)
       distinct
       (filter #{3 2})))

(defn cal-checksum [input]
  (let [all    (reduce concat
                       (for [line input]
                         (count-repeated-letters line)))
        threes (count (filter #{3} all))
        twos   (count (filter #{2} all))]
    (* threes twos)))

(comment
  (cal-checksum input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn diff-strings [s1 s2]
  (->> s2
       (interleave s1)
       (partition-all 2)
       (map distinct)
       (map count)
       (filter #{2})
       count))

(defn common-letters [[s1 s2]]
  (->> s2
       (interleave s1)
       (partition-all 2)
       (map distinct)
       (remove #(= (count %) 2))
       flatten
       (apply str)))

(defn find-pair [input]
  (loop [[s & rest] input]
    (when s
      (let [compare
            (for [i (remove #{s} input)
                  :when (= 1 (diff-strings s i))]
              [s i])]
        (if (seq compare) (common-letters (first compare))
            (recur rest))))))

(comment
  (find-pair input))

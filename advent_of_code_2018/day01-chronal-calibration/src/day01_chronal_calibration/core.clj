(ns day01-chronal-calibration.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input]
  (map parse-long (str/split-lines input)))

(defn calculate-frequency [input]
  (let [steps (parse-input input)]
    (reduce (fn [acc e]
              (+ acc e))
            0
            steps)))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn find-repeating-frequency [nums]
  (loop [[n & ns] nums
         fq #{0}
         active-fq 0]
    (if n
      (let [n-fq (+ active-fq n)
            new-list (conj fq active-fq)]
        (if (new-list n-fq) n-fq
            (recur ns new-list n-fq)))
      (recur nums fq active-fq))))

(comment
  (find-repeating-frequency (parse-input input)))

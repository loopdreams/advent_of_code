(ns day01-trebuchet.core
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "input.txt")))

(defn calibration-values [input]
  (map (fn [line]
         (let [digits (re-seq #"\d" line)]
           (parse-long (str (first digits) (last digits)))))
       input))

(defn sum-of-calibrations [input]
  (apply + (calibration-values input)))

(comment
  (sum-of-calibrations input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(def number-words {"one" 1
                   "two" 2
                   "three" 3
                   "four" 4
                   "five" 5
                   "six" 6
                   "seven" 7
                   "eight" 8
                   "nine" 9})

(defn include-number-words [line]
  (let [[left]  (re-seq (re-pattern (str (str/join #"|" (keys number-words)) "|\\d")) line)
        [right] (->> line
                     str/reverse
                     (re-seq
                      (re-pattern (str (str/join #"|" (map str/reverse (keys number-words))) "|\\d"))))]
    
    (parse-long
     (str
      (number-words left left)
      (number-words (str/reverse right) right)))))

(comment
  (apply + (map include-number-words input)))


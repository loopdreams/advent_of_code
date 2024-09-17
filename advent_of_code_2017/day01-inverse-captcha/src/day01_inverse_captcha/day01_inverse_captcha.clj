(ns day01-inverse-captcha.day01-inverse-captcha
  (:require [clojure.string :as str]))

(def input (str/replace (slurp "D01.txt") "\n" ""))

(defn counter [acc [a b]]
  (if (= a b)
    (+ acc (parse-long (str a)))
    acc))

(defn first-last [input]
  (if (= (first input) (last input))
    (parse-long (str (first input)))
    0))

(defn part-1 [input]
  (+ (first-last input)
     (reduce counter 0 (->> input (partition 2 1)))))

(part-1 input)

;; Part 2

(defn counter-2 [input]
  (let [len  (count input)
        step (int (/ len 2))]
    (reduce (fn [acc i]
              (if (= (nth input i)
                     (nth input (mod (+ i step) len)))
                (+ acc (parse-long (str (nth input i))))
                acc))
            0
            (range len))))

(counter-2 input)

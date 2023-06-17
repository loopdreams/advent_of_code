(ns day10-crt.core
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(defn sig-part [string]
  (let [value (re-find #"[-]?\d+" string)
        type (keyword (re-find #"\w+" string))]
    (if value
      [type (Integer/parseInt value)]
      [type 0])))

(def signal (map sig-part (s/split-lines (slurp "input.txt"))))

(def check-at [20 60 100 140 180 220])

(defn process-signal [signal]
  (loop [cycle 1
         x 1
         [s & rest] signal
         result []]
    (if (empty? s) result
        (case (first s)
          :noop (recur
                 (inc cycle)
                 (+ x (second s))
                 rest
                 (if (some #{cycle} check-at)
                   (conj result (* cycle x))
                   result))
          :addx (recur
                 (inc cycle)
                 x
                 (cons [:noop (second s)] rest)
                 (if (some #{cycle} check-at)
                   (conj result (* cycle x))
                   result))))))

(defn -main
  [& _]
  (let [processed (process-signal signal)
        checked-cycles (zipmap (map #(keyword (str %)) check-at)
                               processed)
        sum (apply + processed)]
    (printf "Values at checked cycles:\n")
    (pp/pprint checked-cycles)
    (printf "Sum of values: %d\n" sum)
    sum))

;;; PART 2

(defn check-pix [pix x]
  (let [newline? (if (= pix 39) "\n")]
    (if (or (= pix (dec x))
            (= pix (inc x))
            (= pix x))
      (str "#" newline?)
      (str "." newline?))))

(defn draw-screen [signal]
  (loop [pix 0
         cycle 1
         x 1
         [s & rest] signal
         screen nil]
    (if (empty? s) screen
        (case (first s)
          :noop (recur
                 (if (= pix 39) 0 (inc pix))
                 (inc cycle)
                 (+ x (second s))
                 rest
                 (str screen (check-pix pix x)))
          :addx (recur
                 (if (= pix 39) 0 (inc pix))
                 (inc cycle)
                 x
                 (cons [:noop (second s)] rest)
                 (str screen (check-pix pix x)))))))

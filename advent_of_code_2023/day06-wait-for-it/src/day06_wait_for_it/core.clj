(ns day06-wait-for-it.core
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn parse-input [input]
  (let [[time distance] (->> (str/split-lines input)
                             (map #(str/split % #":"))
                             (map second)
                             (map #(str/split % #" ")))
        times           (map parse-long (remove empty? time))
        distances       (map parse-long (remove empty? distance))]
    (partition 2 (interleave times distances))))


;; distance = seconds * (time - seconds)
;; d = x * (t - x)
;; d = x * t - x^2
;; x^2 - x * t + d = 0
;;
;;
;; x = t +/- sqrt(t^2 - 4d) / 2

(defn calculate-seconds-given-distance [[time distance]]
  (let [f  (fn [d t plus-minus]
             (/ (plus-minus t
                            (math/sqrt (- (* t t) (* 4 d))))
                2))
        x1 (inc (math/floor (f distance time -)))
        x2 (dec (math/ceil (f distance time +)))]
    [x1 x2]))


(defn part-1 [input]
  (let [races (parse-input input)]
    (reduce *
            (reduce (fn [results race]
                      (let [[x1 x2]
                            (calculate-seconds-given-distance race)]
                        (conj results
                              (int (inc (- x2 x1))))))
                    [] races))))

(comment
  (part-1 input))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn parse-input-part-2 [input]
  (let [races (parse-input input)
        f (fn [pos] (->> (map pos races)
                         (map str)
                         str/join
                         parse-long))
        time (f first)
        dist (f second)]
    [time dist]))

(comment
  (->> (calculate-seconds-given-distance (parse-input-part-2 input))
       reverse
       (apply -)
       inc
       int))

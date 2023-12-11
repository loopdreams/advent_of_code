(ns day11-cosmic-expansion.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-galaxies [input]
  (let [lines (str/split-lines input)]
    (reduce concat
            (for [y (range (count lines))]
              (for [x (range (count (first lines)))
                    :let [point (nth (nth lines y) x)]
                    :when (= point \#)]
                   [x y])))))

(defn empty-rows [lines]
  (for [y (range (count lines))
        :let [line (nth lines y)]
        :when (every? #(= \. %) (seq line))]
    y))

(defn empty-cols [lines]
  (loop [l lines
         empties []
         x 0]
    (if (every? empty? l) empties
        (let [col (map first l)]
          (recur
           (map rest l)
           (if (every? #(= \. %) col)
             (conj empties x)
             empties)
           (inc x))))))

(defn taxi-distance [[x1 y1] [x2 y2]]
  (let [a (abs (- x1 x2))
        b (abs (- y1 y2))]
    (+ a b)))

(defn expand-space [input]
  (let [lines (str/split-lines input)
        ys (empty-rows lines)
        xs (empty-cols lines)]
    [xs ys]))

(defn calculate-distance [[x1 x2 :as g1] [y1 y2 :as g2] [xs ys] multiplier]
  (let [x-path (apply range (sort [x1 y1]))
        y-path (apply range (sort [x2 y2]))
        inc-fn (fn [path expand]
                 (->> path
                      (map #(some #{%} expand))
                      (remove nil?)
                      count))
        x-inc (inc-fn x-path xs)
        y-inc (inc-fn y-path ys)]
    (+ (taxi-distance g1 g2) (* x-inc multiplier) (* y-inc multiplier))))

(defn calculate-distances [galaxies [xs ys] multiplier]
  (loop [[g & gs] galaxies
         distances []]
    (if-not g (reduce + (flatten distances))
            (recur
             gs
             (conj distances
                   (map #(calculate-distance g % [xs ys] multiplier) gs))))))


(comment
  ;; Part 1
  (calculate-distances (parse-galaxies input) (expand-space input) 1)
  ;; Part 2
  (println
   (calculate-distances (parse-galaxies input) (expand-space input) 999999)))

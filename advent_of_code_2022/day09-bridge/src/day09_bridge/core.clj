(ns day09-bridge.core
  (:require [clojure.string :as s]))

(def path-input
  (let [path (s/split-lines (slurp "path.txt"))
        counts (->> path
                    (map #(re-find #"\d+" %))
                    (map #(Integer/parseInt %)))
        directions (->> path
                        (map #(re-find #"\w+" %))
                        (map keyword))]
    (into [] (interleave directions counts))))

(defn steps [direction count [x y]]
  (case direction
    :R (map #(conj [%] y) (range (inc x) (+ x count 1)))
    :L (map #(conj [%] y) (reverse (range (- x count) x)))
    :U (map #(conj [x] %) (range (inc y) (+ y count 1)))
    :D (map #(conj [x] %) (reverse (range (- y count) y)))))

(defn head-path [path]
  (loop [[direction count & rest] path
         result [[0 0]]]
    (if direction
      (recur
       rest
       (concat result
             (steps direction count (last result))))
      result)))

(defn diagonal? [[x1 y1] [x2 y2]]
  (= (abs (- x1 x2)) (abs (- y1 y2))))

(defn touching? [[x1 y1] [x2 y2]]
  (= 1 (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(defn follow-head [path]
  (loop [p path
         result [[0 0]]]
    (let [[a b] p]
      (if b
        (recur
         (rest p)
         (if (or (diagonal? (last result) b)
                 (touching? (last result) b))
           result
           (conj result a)))
        result))))

(defn -main
  [& _]
  (let [h-path (head-path path-input)
        t-path (follow-head h-path)
        points-visited (count (set t-path))]
    (printf "%d" points-visited)
    points-visited))

(defn part-2 [tails]
  (loop [path (head-path path-input)
         t tails]
    (if (zero? t) path
        (recur
         (follow-head path)
         (dec t)))))

(part-2 1)

(ns day05-vents.core
  (:require [clojure.string :as str]))

(defn vert-linepoints [[a1 b1] [a2 b2]]
  (let [[x1 x2] (sort [a1 a2])
        [y1 y2] (sort [b1 b2])]
    (cond
      (= x1 x2) (for [y (range y1 (inc y2))]
                  [x1 y])
      (= y1 y2) (for [x (range x1 (inc x2))]
                  [x y1])
      :else nil)))

(defn parse-input [input]
  (remove nil?
          (for [line (str/split-lines input)
                :let [[x1 y1 x2 y2] (map #(Integer/parseInt %)
                                         (re-seq #"-?\d+" line))]]
            (vert-linepoints [x1 y1] [x2 y2]))))


(defn intersections [input]
  (let [lines (parse-input input)]
    (count (->> lines
                (reduce concat)
                (frequencies)
                (filter (fn [[k v]] (> v 1)))
                (keys)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn diagonal? [[x1 y1] [x2 y2]]
   (/ (- y2 y1)
      (- x2 x1)))

(defn draw-dia [x-range y-range]
  (->> (interleave x-range y-range)
       (partition 2)
       (map vec)))

(defn more-linepoints [[a1 b1] [a2 b2]]
  (let [[x1 x2] (sort [a1 a2])
        [y1 y2] (sort [b1 b2])
        x-range (range x1 (inc x2))
        y-range (range y1 (inc y2))]
    (cond
      (= x1 x2) (for [y y-range] [x1 y])
      (= y1 y2) (for [x x-range] [x y1])
      (=  1  (diagonal? [a1 b1] [a2 b2])) (draw-dia x-range y-range)
      (= -1  (diagonal? [a1 b1] [a2 b2])) (draw-dia x-range (reverse y-range))
      :else nil)))

(defn parse-input-part-2 [input]
  (remove nil?
          (for [line (str/split-lines input)
                :let [[x1 y1 x2 y2] (map #(Integer/parseInt %)
                                         (re-seq #"-?\d+" line))]]
            (more-linepoints [x1 y1] [x2 y2]))))

(defn more-intersections [input]
  (let [lines (parse-input-part-2 input)]
    (count (->> lines
                (reduce concat)
                (frequencies)
                (filter (fn [[k v]] (> v 1)))
                (keys)))))

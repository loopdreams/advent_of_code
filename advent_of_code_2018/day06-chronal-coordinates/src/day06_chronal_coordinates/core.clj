(ns day06-chronal-coordinates.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input]
  (for [l (str/split-lines input)
        :let [coords (mapv (comp parse-long str/trim)
                           (str/split l #","))]]
    coords))

(defn grid-size [coords]
  (let [x (last (sort (map first coords)))
        y (last (sort (map second coords)))]
    [x y]))

(defn taxi-distance [[x1 y1] [x2 y2]]
  (let [a (abs (- x1 x2))
        b (abs (- y1 y2))]
    (+ a b)))

(defn map-distances [coords point]
  (reduce #(assoc %1 %2 (taxi-distance point %2)) {} coords))

(defn select-closest
  "Return nil if equidistant to 2 points"
  [ps]
  (let [[a b & _] ps]
    (when (not= (val a) (val b))
      (key a))))

(defn find-closest-point [coords point]
  (->>
   (map-distances coords point)
   (sort-by val)
   select-closest))

(defn remove-edges [breadth depth m]
  (into {}
        (remove (fn [[_ v]]
                  (some #{0 breadth depth} (flatten v)))
                m)))

(defn map-areas [input]
  (let [coords (parse-input input)
        [breadth depth] (grid-size coords)
        grid
        (for [x (range breadth)
              y (range depth)]
          [x y])]
    (->>
     (reduce (fn [result point]
               (let [closest-point (find-closest-point coords point)]
                 (update result closest-point
                         (fnil conj [])
                         point)))
             {}
             grid)
     (remove-edges breadth depth)
     (map #(count (val %)))
     sort
     last)))

(comment
  (map-areas input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn within-range? [distances]
  (when (> 10000
           (apply + (vals distances)))
    true))

(defn size-safe-region [input]
  (let [coords          (parse-input input)
        [breadth depth] (grid-size coords)
        grid            (for [x (range breadth)
                              y (range depth)]
                          [x y])]
    (reduce (fn [acc point]
              (if (within-range? (map-distances coords point))
                (inc acc)
                acc))
            0
            grid)))

(comment
  (size-safe-region input))

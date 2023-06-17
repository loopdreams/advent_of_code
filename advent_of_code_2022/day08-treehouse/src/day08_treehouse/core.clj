(ns day08-treehouse.core
  (:require [clojure.string :as s]))

(def tree-grid (s/split-lines (slurp "input.txt")))
(def example-grid (s/split-lines (slurp "tree-grid.txt")))

(defn lookup [[x y] tree-grid]
  (Integer/parseInt (str (nth (nth tree-grid y) x))))

(defn count-outer-visible [tree-grid]
  (let [outer-rows (* (count (first tree-grid)) 2)
        inner-rows (* (- (count tree-grid) 2) 2)]
    (+ inner-rows outer-rows)))

(defn inner-tree-coords [tree-grid]
  (let [xs (range 1 (dec (count (first tree-grid))))
        ys (range 1 (dec (count tree-grid)))]
    (for [x xs
          y ys]
      [x y])))

(defn all-cords [tree-grid]
  (let [xs (range 1 (count (first tree-grid)))
        ys (range 2 (count tree-grid))]
    (for [x xs
          y ys]
      [x y])))

(defn aligned-trees [[x y] tree-grid]
  (let [[north _ south] (partition-by #(= % y)
                                      (range 0 (count tree-grid)))
        [west _ east] (partition-by #(= % x)
                                    (range 0 (count (first tree-grid))))
        norths (map #(conj [x] %) north)
        souths (map #(conj [x] %) south)
        easts (map #(conj [%] y) east)
        wests (map #(conj [%] y) west)]
    (zipmap [:n :s :e :w] [norths souths easts wests])))

(defn visible? [point direction tree-grid]
  (loop [t (lookup point tree-grid)
         ts (map #(lookup % tree-grid) direction)]
    (if (empty? ts) true
        (if (>= (first ts) t) false
            (recur t (rest ts))))))

(defn count-inner-visible [tree-grid]
  (let [in-trees (inner-tree-coords tree-grid)]
    (loop [[t & ts] in-trees
           result 0]
      (if (empty? t) result
          (let [aligned (aligned-trees t tree-grid)]
            (if (or
                 (visible? t (:n aligned) tree-grid)
                 (visible? t (:s aligned) tree-grid)
                 (visible? t (:e aligned) tree-grid)
                 (visible? t (:w aligned) tree-grid))
              (recur ts (inc result))
              (recur ts result)))))))

(defn -main
  [& _]
  (let [outer (count-outer-visible tree-grid)
        inner (count-inner-visible tree-grid)]
    (+ inner outer)))

;; Part 2

(defn count-viewable-trees [point direction tree-grid]
  (if (empty? direction) 0
      (loop [t (lookup point tree-grid)
             ts (map #(lookup % tree-grid) direction)
             c 0]
        (cond
          (empty? ts) c
          (>= (first ts) t) (inc c)
          :else (recur
                 t
                 (rest ts)
                 (inc c))))))

(defn count-best-view [tree-grid]
  (loop [[t & ts] (all-cords tree-grid)
         result []]
    (if (empty? t) (reverse (sort  result))
        (let [aligned (aligned-trees t tree-grid)
              north (count-viewable-trees t (reverse (:n aligned)) tree-grid)
              south (count-viewable-trees t (:s aligned) tree-grid)
              west (count-viewable-trees t (reverse (:w aligned)) tree-grid)
              east (count-viewable-trees t (:e aligned) tree-grid)
              sum (* north south east west)]
          (recur ts (conj result sum))))))

(defn part2 []
  (let [best-view (first (count-best-view tree-grid))]
    (println best-view)
    best-view))


;; additional:
;;
(defn random-tree-grid [width height]
  (let [rowfn (fn [x] (reduce str (take x (repeatedly #(rand-int 10)))))]
    (loop [n height
           result []]
      (if (zero? n) result
          (recur
           (dec n)
           (conj result (rowfn width)))))))

(defn count-random-hidden [w h]
  (let [tree-grid (random-tree-grid w h)
        total-trees (* w h)
        outer (count-outer-visible tree-grid)
        inner (count-inner-visible tree-grid)]
    (- total-trees (+ outer inner))))

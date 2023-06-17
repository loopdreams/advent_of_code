(ns day09-smoke-basin.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (into [] (for [l lines]
               (into [] (map #(Integer/parseInt %)
                             (re-seq #"\d" l)))))))

(defn check-surrounding [map line idx val]
  (let [above                 (when (pos? line) (- line 1))
        below                 (when (< (inc line) (count map)) (inc line))
        left                  (when (pos? idx) (- idx 1))
        right                 (when (< (inc idx) (count (first map))) (inc idx))]
    (and
     (if above (< val (nth (nth map above) idx)) true)
     (if below (< val (nth (nth map below) idx)) true)
     (if right (< val (nth (nth map line) right)) true)
     (if left  (< val (nth (nth map line) left)) true))))

(defn find-basin-vals [points]
    (flatten (for [i (range 0 (count points))]
               (for [j (range 0 (count (first points)))
                     :let [val (nth (nth points i) j)]
                     :when (check-surrounding points i j val)]
                 val))))

(defn risk-level [input]
  (->> input
       (parse-input)
       (find-basin-vals)
       (map inc)
       (apply +)))

(comment (risk-level input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn map-points
  "Removes all coords for '9's"
  [input]
  (let [points (parse-input input)]
    (mapcat (partial map merge)
            (for [i (range 0 (count points))]
              (for [j (range 0 (count (first points)))
                    :let [val (nth (nth points i) j)]
                    :when (< val 9)]
                {:coords [i j] :val val})))))

(defn neighbours [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]]))

(defn basin-size
  ([queue] (basin-size (set (rest queue)) [(first queue)] 0))
  ([queue basin-queue size]
   (if (empty? basin-queue) [size queue]
       (let [neighbours (neighbours (first basin-queue))
             connected  (remove nil? (map queue neighbours))]
         (if (empty? connected)
           (recur queue (rest basin-queue) (inc size))
           (recur (set (remove (set connected) queue))
                  (concat (rest basin-queue) connected)
                  (inc size)))))))

(defn count-basin-sizes [input]
  (let [points (map-points input)
        queue (set (map :coords points))]
    (loop [q queue
           basins []]
      (if (empty? q) (->> basins
                          (sort)
                          (reverse)
                          (take 3)
                          (reduce *))
          (let [[size new-queue] (basin-size q)]
            (recur new-queue (conj basins size)))))))


(comment
  (count-basin-sizes input))

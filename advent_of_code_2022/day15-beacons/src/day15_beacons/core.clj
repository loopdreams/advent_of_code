(ns day15-beacons.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]))

(defn get-coords [line]
  (->> line
       (re-seq #"=(-?\d+)")
       (map second)
       (map #(Integer/parseInt %))
       (partition 2)))

(defn parse-input [input]
  (let [coords (->> input
                    (str/split-lines)
                    (map get-coords))]
    (loop [[[sensor beacon] & rest] coords
           locations []]
      (if beacon
        (recur rest (conj locations
                              {:sensor (into [] sensor)
                               :beacon (into [] beacon)}))
        locations))))

(defn taxi-distance [[x1 y1] [x2 y2]]
  (let [a (abs (- x1 x2))
        b (abs (- y1 y2))]
    (+ a b)))

(defn edges [[s1 s2] radius line rlimit llimit]
  (let [distance (abs (- radius (abs (- s2 line))))
        left (- s1 distance)
        right (+ s1 distance)]
    [(if (> left llimit) left 0) (if (< right rlimit) right rlimit)]))

(defn sensor-coverage [sensor beacon line rlimit llimit]
  (let [[_ s2] sensor
        radius (taxi-distance sensor beacon)]
    (when (> radius (abs (- s2 line)))
      (edges sensor radius line rlimit llimit))))

(defn combine-blocked-ranges
  "Taken from https://github.com/abyala/advent-2022-clojure/blob/main/docs/day15.md"
  [ranges]
  (reduce (fn [acc [low' high' :as r]]
            (let [[low high] (last acc)]
              (cond
                (nil? low) [r]
                (<= low' (inc high)) (update-in acc [(dec (count acc)) 1] max high')
                :else (conj acc r))))
          []
          (sort ranges)))

(defn coverage-line [locations line rlimit llimit]
    (loop [[p & points] locations
           at-line []]
      (if p
        (recur points
               (conj at-line (sensor-coverage (:sensor p) (:beacon p) line rlimit llimit)))
        (combine-blocked-ranges at-line))))

(defn part1 [line input rlimit llimit]
  (let [locations (parse-input input)
        beacon-locs (map #(:beacon %) locations)
        beacons-at-line (count (set (map first (filter #(= (second %) line) beacon-locs))))
        [left right] (first (coverage-line locations line rlimit llimit))]
    (println (-  (count (range left (inc right))) beacons-at-line))))

(comment
  (part1 2000000 (slurp "input.txt") 80000000 -8000000)
  (part1 10 (slurp "sample.txt") 50 -10))

;;; PART 2
;;;

;; (defn scanner [xlimit ylimit input]
;;   (let [compare (set (range 0 xlimit))
;;         locations (parse-input input)
;;         line (atom nil)]
;;     (loop [y ylimit]
;;       (if (zero? y) "Not found"
;;           (if (empty? @line)
;;             (let [coverage (coverage-line2 locations y xlimit)]
;;               (if (= compare coverage)
;;                 (recur (dec y))
;;                 (do
;;                   (reset! line [(first (set/difference compare coverage)) y])
;;                   (recur (dec y)))))
;;             (let [[x y]  @line]
;;               (println x y)
;;               (+ y (* x 4000000))))))))

(defn scanner [xlimit ylimit input]
  (let [locations (parse-input input)]
    (first (for [y (range 0 (inc ylimit))
                 :let [coverage (coverage-line locations y (inc xlimit) 0)]
                 :when (= 2 (count coverage))
                 :let [[[_ x1] [x2 _]] coverage
                       [_ x _] (range x1 (inc x2))]]
             (+ (* x 4000000) y)))))

(defn part2 [xlim ylim input]
    (println (scanner xlim ylim input)))

(comment
  (part2 40 40 (slurp "sample.txt"))
  (part2 4000000 4000000 (slurp "input.txt")))

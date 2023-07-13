(ns day12-hills.core
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def sample (slurp "elevations.txt"))
(def input (slurp "input.txt"))

(defn line-to-map [line y-val]
  (let [xs     (range 0 (count line))
        coords (->> y-val
                    repeat
                    (interleave xs)
                    (partition 2)
                    (map vec))]
    (into {} (map (fn [coord val]
                    [coord val])
                  coords
                  line))))

(defn parse-input [input]
  (let [lines  (str/split-lines input)
        height (count lines)
        vals   (for [l lines]
                 (mapv (fn [char]
                         (cond
                           (= char \S) 0
                           (= char \E) 27
                           :else (- (int char) 96))) l))]
    (loop [c 0
           v vals
           m {}]
      (if (= c height) m
          (recur (inc c)
                 (rest v)
                 (into m (line-to-map (first v) c)))))))



(def ^:private inf (Long/MAX_VALUE))

(defn neighbours [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]]))

(defn valid-nbrs [point queue m]
  (let [p-val (get m point)]
    (->> point
         neighbours
         (filter queue)
         (select-keys m)
         (filter (fn [[_ v]] (<= v (inc p-val)))))))


(defn find-shortest-path [m start end]
    (loop [queue (assoc (->> (repeat inf)
                             (interleave (keys m))
                             (partition 2)
                             (map vec)
                             (into (priority-map)))
                        start 0)]
      (if (empty? queue) "Not Found"

          (let [[point curr-cost] (peek queue)]
            (if (= point end) curr-cost

                (let [nbrs (valid-nbrs point queue m)

                      new-queue (reduce (fn new-queue [q [nbr _]]
                                          (update q nbr (partial min (+ curr-cost 1))))
                                        (pop queue)
                                        nbrs)]

                  (recur new-queue)))))))

(defn part-1 [input]
  (let [m         (parse-input input)
        [start _] (first (filter (fn [[_ v]] (= v 0)) m))
        [end _]   (first (filter (fn [[_ v]] (= v 27)) m))]
    (find-shortest-path m start end)))

(comment
  (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(defn valid-nbrs-reverse [point queue m]
  (let [p-val (get m point)]
    (->> point
         neighbours
         (filter queue)
         (select-keys m)
         (filter (fn [[_ v]] (>= v (dec p-val))))))) ;; part 2 changed this line


(defn find-shortest-path-reverse [m start end]
    (loop [queue (assoc (->> (repeat inf)
                             (interleave (keys m))
                             (partition 2)
                             (map vec)
                             (into (priority-map)))
                        start 0)]
      (if (empty? queue) "Not Found"

          (let [[point curr-cost] (peek queue)]
            (if (= (get m point) end) curr-cost ;; part 2 changed this line

                (let [nbrs (valid-nbrs-reverse point queue m)

                      new-queue (reduce (fn new-queue [q [nbr _]]
                                          (update q nbr (partial min (+ curr-cost 1))))
                                        (pop queue)
                                        nbrs)]

                  (recur new-queue)))))))

(defn part-2 [input]
  (let [m (parse-input input)
        [start _] (first (filter (fn [[_ v]] (= v 27)) m))]
    (find-shortest-path-reverse m start 1)))

(comment (part-2 input))

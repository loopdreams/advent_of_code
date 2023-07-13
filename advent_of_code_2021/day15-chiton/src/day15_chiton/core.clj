(ns day15-chiton.core
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def sample (slurp "sample.txt"))
(def input  (slurp  "input.txt"))

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
                 (mapv (comp parse-long str) l))]
    (loop [c 0
           v vals
           m {}]
      (if (= c height) m
          (recur (inc c)
                 (rest v)
                 (into m (line-to-map (first v) c)))))))


;; Dijkstra's:
(def ^:private inf (Long/MAX_VALUE))

(defn neighbours [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]]))

(defn find-shortest-path [m start end]
  (loop [p-queue (assoc (->> (repeat inf) ;; init costs
                             (interleave (keys m))
                             (partition 2)
                             (map vec)
                             (into (priority-map)))
                        start 0)]       ;; Set start cost to zero (and this moves point to front of map)
    (if (empty? p-queue) "Not Found"
        (let [[point curr-cost] (peek p-queue)]
          (if (= point end) curr-cost
              (let [nbrs      (->> point neighbours (filter p-queue) (select-keys m))
                    new-queue (reduce (fn new-queue [q [nbr nbr-cost]]
                                        (update q nbr (partial min (+ curr-cost nbr-cost))))
                                      (pop p-queue)
                                      nbrs)]

                (recur new-queue)))))))
;;;


(defn part-1 [input]
  (let [m (parse-input input)
        start [0 0]
        end (-> m keys sort last)]
    (find-shortest-path m start end)))

(comment
  (time (println (part-1 sample))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(defn expand-down [point length]
  (map (partial mapv + point) [[0 0] [0 length] [0 (* 2 length)] [0 (* 3 length)] [0 (* 4 length)]]))

(defn expand-right [point length]
  (map (partial mapv + point) [[0 0] [length 0] [(* 2 length) 0] [(* 3 length) 0 ] [(* 4 length) 0]]))

(defn expand-map-dir [m dir len]
  (reduce (fn [new-m [k v]]
            (let [new-points (dir k len)
                  new-vals (take (count new-points)
                                 (iterate #(inc (mod % 9)) v))]
              (into new-m (zipmap new-points new-vals))))
          {}
          m))

(defn expand-map [m]
  (let [[height width] (-> m keys sort last)
        down (expand-map-dir m expand-down (inc height))
        full (expand-map-dir down expand-right (inc width))]
    full))

(defn part-2 [input]
  (let [m (expand-map (parse-input input))
        start [0 0]
        end (-> m keys sort last)]
    (find-shortest-path m start end)))

(comment
  (time (println (part-2 input))))

;; time (sample):
;; "Elapsed time: 1116.431833 msecs"
;; Using priority map:
;; "Elapsed time: 69.365166 msecs"

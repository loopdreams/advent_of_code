(ns day15-chiton.core
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def sample (slurp "sample.txt"))
(def input  (slurp  "input.txt"))

(defn neighbours [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]]))

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


(def ^:private inf (Long/MAX_VALUE))


;; adapted from https://gist.github.com/loganlinn/5437067
(defn update-costs [m costs point queue]
  (let [current-cost (get costs point)
        nbr-coords (neighbours point)]
    (reduce (fn [cs [nbr nbr-cost]]
              (update cs nbr (partial min (+ current-cost nbr-cost))))
            costs
            (select-keys m (filter queue nbr-coords)))))

(update-costs {[0 0] 0, [0 1] 8, [0 2] 4} (priority-map [0 0] 0 [0 1] 100 [0 2] 100) [0 0] #{[0 1] [0 2]})
(sort-by {[0 0] 0, [0 1] 8, [0 2] 100} #{[0 2] [0 1]})

(defn find-path [m start end]
  (loop [costs (assoc (->> inf
                           repeat
                           (interleave (keys m))
                           (partition 2)
                           (map vec)
                           (into {}))
                      start 0)
         queue (disj (apply hash-set (keys m)) start)
         p start]
    (if (or (empty? queue) (= inf (costs p)))
      (get costs end)
      (let [new-costs (update-costs m costs p queue)
            next-p (first (sort-by new-costs queue))]
        (if (= end p) (get new-costs p)
            (recur
             new-costs
             (disj queue next-p)
             next-p))))))

(sort-by {:a 1 :b 2} #{:c :d :a})
               

(defn part-1 [input]
  (let [m (parse-input input)
        start [0 0]
        end (-> m keys sort last)]
    (find-path m start end)))

(comment
  (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(defn expand-down [point length]
  (map (partial mapv + point) [[0 0] [0 length] [0 (* 2 length)] [0 (* 3 length)] [0 (* 4 length)]]))

(defn expand-right [point length]
  (map (partial mapv + point) [[0 0] [length 0] [(* 2 length) 0] [(* 3 length) 0 ] [(* 4 length) 0 ]]))

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
        right (expand-map-dir down expand-right (inc width))]
    right))

(defn part-2 [input]
  (let [m (expand-map (parse-input input))
        start [0 0]
        end (-> m keys sort last)]
    (find-path m start end)))

(comment
  (time (println (part-2 sample))))

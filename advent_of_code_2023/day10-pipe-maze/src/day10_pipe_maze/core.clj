(ns day10-pipe-maze.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(def symbols-to
  {:east  [\- \J \7]
   :west  [\- \L \F]
   :north [\| \F \7]
   :south [\| \L \J]})

(def symbols-from
  {:east  [\- \L \F]
   :west  [\- \J \7]
   :north [\| \L \J]
   :south [\| \F \7]})

(defn connects? [point connection direction]
  (if (= point \S)
    (some #{connection} (direction symbols-to))
    (and (some #{point} (direction symbols-from))
         (some #{connection} (direction symbols-to)))))

(defn parse-row [r y]
  (into {}
        (for [x (range (count r))
              :let [char (nth r x)]]
          [[x y] char])))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (apply merge
          (for [y (range (count lines))
                :let [row (nth lines y)]]
            (parse-row row y)))))

(defn find-start [m]
  (->>
   (filter #(= (val %) \S) m)
   first
   key))

(defn neighbours
  "South, north, west, east"
  [point]
  (zipmap
   (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]])
   [:south :north :west :east]))

(defn next-step [point m candidates]
  (let [connects (select-keys m (keys candidates))]
    (for [c connects
          :let [[k val] c
                dir (candidates k)]
          :when (connects? point val dir)]
      k)))

(defn path-loop [input]
  (let [m (parse-input input)
        start (find-start m)
        [s1 end] (next-step (m start) m (neighbours start))]
    (loop [p s1
           path [start]]
      (when p
        (if-not (= p end)
          (let [nbrs (apply dissoc (neighbours p) path)
                next-p (first (next-step (m p) m nbrs))]
            (recur
             next-p
             (conj path p)))
          path)))))

(defn part-1 [input]
  (inc (int
        (/ (count (path-loop input))
           2))))

(comment
  (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

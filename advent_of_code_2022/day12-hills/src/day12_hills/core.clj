(ns day12-hills.core
  (:require [clojure.string :as str]))

(defn parse-to-char-coords
  "Given an input string of a multi-line grid of single characters, returns a lazy sequence of [[x y] c] tuples of
  [x y] coords to each character c. If the function f is provided, it transforms each value c using that function."
  ([input] (parse-to-char-coords identity input))
  ([f input] (->> (str/split-lines input)
                  (map-indexed (fn [y line]
                                 (map-indexed (fn [x c] [[x y] (f c)]) line)))
                  (apply concat))))

(defn parse-to-char-coords-map
  "Given an input string of a multi-line grid of single characters, returns a map of {[x y] c} mapping the [x y]
  coordinates to each character c. If the function f is provided, it transforms each value c using that function."
  ([input] (parse-to-char-coords-map identity input))
  ([f input] (into {} (parse-to-char-coords f input))))


(defn elevation [c]
  (or ({\S 1, \E 26} c)
      (- (int c) 96)))

(defn filter-for-keys [value-pred coll]
  (keep (fn [[k v]] (when (value-pred v) k)) coll))

(defn parse-input [starting-chars input]
  (let [grid (parse-to-char-coords-map input)]
    {:grid (reduce-kv #(assoc %1 %2 (elevation %3)) {} grid)
     :starts (filter-for-keys starting-chars grid)
     :target (first (filter-for-keys #{\E} grid))}))

(parse-input #{\S} (slurp "elevations.txt"))

(defn neighbors [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]]))

(defn climbable? [grid from to]
  (<= (grid to) (inc (grid from))))

(defn possible-steps [grid current]
  (->> (neighbors current)
       (filter grid)
       (filter (partial climbable? grid current))))

(defn shortest-path [grid target start]
  (loop [options [{:current start, :moves 0}]
         seen #{}]
    (let [{:keys [current moves]} (first options)]
      (cond
        (= current target) moves
        (seen current) (recur (subvec options 1) seen)
        :else (let [neighbors (possible-steps grid current)
                    neighbor-options (map #(hash-map :current %, :moves (inc moves)) neighbors)
                    combined-options (apply conj options neighbor-options)]
                (when (seq combined-options)
                  (recur (subvec (apply conj options neighbor-options) 1) (conj seen current))))))))

(defn part1 [input]
  (let [{:keys [grid starts target]} (parse-input #{\S} input)]
    (shortest-path grid target (first starts))))

(part1 (slurp "input.txt"))
(subvec [{:current 1, :moves 0}] 1)

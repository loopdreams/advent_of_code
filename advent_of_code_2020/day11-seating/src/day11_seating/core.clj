(ns day11-seating.core
  (:require
    [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-line [line y]
  (let [chars (map-indexed vector (seq line))]
    (map (fn [[idx char]]
           (if (= char \L)
             {[y idx] :empty}
             {[y idx] :floor}))
         chars)))

(defn parse-input [input]
  (let [lines (->> input str/split-lines)]
    (loop [[l & rest] lines
           c 0
           seats {}]
      (if l (recur rest  (inc c) (into seats (parse-line l c)))
          seats))))

(defn surrounding
  "Taken from https://github.com/abyala/advent-2022-clojure/blob/main/src/advent_2022_clojure/point.clj"
  ([point] (surrounding false point))
  ([include-self? point] (let [points (if include-self? [[-1 -1] [0 -1] [1 -1] [-1 0] [0 0] [1 0] [-1 1] [0 1] [1 1]]
                                          [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])]
                           (map (partial mapv + point) points))))

(defn count-occupied [seats]
  (->> seats
       vals
       (filter #{:occupied})
       count))

(defn filter-occupied [to-check seats]
  (filter (fn [e] (#{:occupied} (get seats e))) to-check))

(defn update-seats [seats]
  (into {} (for [s seats
                 :let [state (val s)
                       surrounding (surrounding (key s))
                       occupied (filter-occupied surrounding seats)]]
             (case state
               :occupied (if (>  (count occupied) 3)
                           {(key s) :empty} s)
               :empty (if (zero? (count occupied))
                        {(key s) :occupied} s)
               :floor s))))

(defn run-simulation [input]
  (let [seats (iterate (partial update-seats) (parse-input input))]
    (loop [n 1]
      (let [next (nth seats n)
            prev (nth seats (dec n))]
        (if (= next prev) (count-occupied next)
            (recur (inc n)))))))



(comment (time (run-simulation input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(def directions #{:up :up-right :right :down-right :down :down-left :left :left-up})

(defn lookup-direction [point seats direction]
  (let [[x1 y1]       point
        check-fn      (fn [dir next]
                        (let [val (get seats next)]
                          (when val
                            (if (#{:occupied :empty} val) val
                                (lookup-direction next seats dir)))))]
    (case direction
      :up         (check-fn direction [x1 (dec y1)])
      :up-right   (check-fn direction [(inc x1) (dec y1)])
      :right      (check-fn direction [(inc x1) y1])
      :down-right (check-fn direction [(inc x1) (inc y1)])
      :down       (check-fn direction [x1 (inc y1)])
      :down-left  (check-fn direction [(dec x1) (inc y1)])
      :left       (check-fn direction [(dec x1) y1])
      :left-up    (check-fn direction [(dec x1) (dec y1)]))))


(defn update-seats-2 [seats]
  (into {} (for [s seats
                 :let [state (val s)
                       occupied (filter #{:occupied}
                                        (map #(lookup-direction (key s) seats %) directions))]]
             (case state
               :occupied (if (>  (count occupied) 4)
                           {(key s) :empty} s)
               :empty (if (zero? (count occupied))
                        {(key s) :occupied} s)
               :floor s))))


(defn run-simulation-2 [input]
  (let [seats (iterate (partial update-seats-2) (parse-input input))]
    (loop [n 1]
      (let [next (nth seats n)
            prev (nth seats (dec n))]
        (if (= next prev) (count-occupied next)
            (recur (inc n)))))))

(comment (run-simulation-2 input))

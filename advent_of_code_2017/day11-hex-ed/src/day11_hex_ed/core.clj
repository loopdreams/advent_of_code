(ns day11-hex-ed.core
  (:require
   [clojure.string :as str]))

(def input (slurp "D11.txt"))

;; Info about hex coords - https://www.redblobgames.com/grids/hexagons/

(defn make-move [dir [q r s]]
  (case dir
    :n  [q (dec r) (inc s)]
    :ne [(inc q) (dec r) s]
    :se [(inc q) r (dec s)]
    :s  [q (inc r) (dec s)]
    :sw [(dec q) (inc r) s]
    :nw [(dec q) r (inc s)]))

(defn cube-subtract [[q1 r1 s1] [q2 r2 s2]]
  [(- q1 q2) (- r1 r2) (- s1 s2)])

(defn cube-distance [p1 p2]
  (let [[a b c] (cube-subtract p1 p2)]
    (/ (+ (abs a) (abs b) (abs c)) 2)))

(defn parse-input [input]
  (map keyword
       (str/split (str/replace input #"\n" "") #",")))

(defn fewest-steps [input]
  (let [steps (parse-input input)
        start [0 0 0]]
    (->>
     (reduce (fn [res step]
               (make-move step res))
             start
             steps)
     (cube-distance start))))

(comment (fewest-steps input))

;; Part 2

(defn furthest-distance [input]
  (let [steps (parse-input input)
        start [0 0 0]]
    (->
     (reduce (fn [{:keys [max pos] :as res} step]
               (let [new-pos (make-move step pos)
                     new-dist (cube-distance start new-pos)]
                 (if (> new-dist max)
                   (-> res
                       (assoc :max new-dist)
                       (assoc :pos new-pos))
                   (assoc res :pos new-pos))))
             {:max 0 :pos start}
             steps)
     :max)))

(comment (furthest-distance input))

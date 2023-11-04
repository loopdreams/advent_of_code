(ns day03-how-you-slice-it.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input]
  (for [line (str/split-lines input)
        :let [[id _ pos size] (str/split line #" ")
              [_ xpos ypos]   (re-find #"(\d+),(\d+)" pos)
              [_ x y]         (re-find #"(\d+)x(\d+)" size)]]
    {:id   id
     :xpos (parse-long xpos)
     :ypos (parse-long ypos)
     :x    (parse-long x)
     :y    (parse-long y)}))

(defn make-grid [x y]
  (for [i (range y)]
    (for [j (range x)]
      [j i])))

(defn make-fabric-coords [{:keys [xpos ypos x y]}]
  (let [area (reduce concat (make-grid x y))]
    (map (partial mapv + [xpos ypos]) area)))

(defn find-overlaps [input]
  (let [coords (reduce concat (for [square (parse-input input)]
                                (make-fabric-coords square)))]
    (->> coords
         sort
         (partition-by identity)
         (remove #(= (count %) 1))
         count)))

(comment
  (find-overlaps input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn overlapping-squares? [sq1-set others]
  (if (seq others)
    (if (seq (set/intersection sq1-set
                               (set (make-fabric-coords (first others)))))
      nil
      (recur sq1-set (rest others)))
    :unique))

(defn find-unique-id [input]
  (let [squares (parse-input input)]
    (loop [[s & rest] squares]
      (if s
        (let [sq1-set (set (make-fabric-coords s))
              overlaps (overlapping-squares?
                        sq1-set
                        (remove #{s} squares))]
          (if-not overlaps
            (recur rest)
            (:id s)))
        "Not Found"))))

(comment
  ;; it's slow...
  (find-unique-id input))

(ns day03-toboggan.core
  (:require
   [clojure.string :as str]
   [clojure.core :as core]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map seq)
       (map vec)))

(defn lookup-pos [m [down right]]
  (let [x-bound (count (first m))]
    (-> m
        (nth down)
        (nth (mod right x-bound)))))

(defn right-vals
  ([] (right-vals 3))
  ([n] (lazy-seq (cons n (right-vals (+ n 3))))))


(defn coords [map]
  (let [ys (range 1 (count map))
        xs (take (count map) (right-vals))]
    (partition 2 (interleave ys xs))))

(defn count-trees [input]
  (let [parsed (parse-input input)
        coords (coords parsed)]
    (->> coords
         (map #(lookup-pos parsed %))
         (filter #{\#})
         (count))))

(comment
  (count-trees input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn incrementor [n]
  (iterate (partial + n) n))

(defn coords-2 [map-count x-inc y-inc]
  (let [ys (take map-count (incrementor y-inc))
        xs (take map-count (incrementor x-inc))]
    (partition 2 (interleave ys xs))))

(defn check-many-slopes [input]
  (let [m (parse-input input)
        map-size (dec (count m))
        filter-fn (fn [m-size x-inc y-inc]
                    (filter #{\#}
                            (map #(lookup-pos m %)
                                 (coords-2 m-size x-inc y-inc))))
        c1 (filter-fn map-size 1 1)
        c2 (filter-fn map-size 3 1)
        c3 (filter-fn map-size 5 1)
        c4 (filter-fn map-size 7 1)
        c5 (filter-fn (/ map-size 2) 1 2)]
    (reduce * (map count [c1 c2 c3 c4 c5]))))

(comment
  (check-many-slopes input))


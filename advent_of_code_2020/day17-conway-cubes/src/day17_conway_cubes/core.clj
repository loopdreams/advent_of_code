(ns day17-conway-cubes.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input  (slurp  "input.txt"))

(defn parse-line [line y-val]
  (for [l (map-indexed vector line)
                  :let [[pos type] l]
                  :when (= type \#)]
              [pos y-val 0]))

(defn parse-input [input]
  (into #{}
        (reduce concat
                (let [lines (str/split-lines input)]
                  (for [i (map-indexed vector lines)
                        :let [[idx line] i]]
                    (parse-line line idx))))))



(defn neighbours [point]
  (let [same-plane (map (partial mapv + point)
                        [[-1 1 0] [0 1 0] [1 1 0]
                         [-1 0 0] [0 0 0] [1 0 0]
                         [-1 -1 0][0 -1 0][1 -1 0]])
        above      (map (fn [[x y z]] [x y (inc z)]) same-plane)
        below      (map (fn [[x y z]] [x y (dec z)]) same-plane)]
    (concat (remove #{point} same-plane) above below)))

(defn keep-active? [active-point active]
  (let [nbrs (neighbours active-point)
        active-neighbours (filter active nbrs)]
    (or (= 3 (count active-neighbours))
        (= 2 (count active-neighbours)))))

(defn make-active? [point active]
  (let [nbrs (neighbours point)
        active-neighbours (filter active nbrs)]
    (= 3 (count active-neighbours))))

(defn cube-cycle [active]
  (let [neighbours (reduce concat (map neighbours active))
        keep-act (filter #(keep-active? % active) active)
        make-act (filter #(make-active? % active) neighbours)]
    (into #{} (concat keep-act make-act))))

(defn part-1 [input]
  (let [active (parse-input input)]
    (->> active
         (iterate cube-cycle)
         (take 7)
         last
         count)))

(comment
  (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn parse-line-2 [line y-val]
  (for [l (map-indexed vector line)
                  :let [[pos type] l]
                  :when (= type \#)]
              [pos y-val 0 0]))

(defn parse-input-2 [input]
  (into #{}
        (reduce concat
                (let [lines (str/split-lines input)]
                  (for [i (map-indexed vector lines)
                        :let [[idx line] i]]
                    (parse-line-2 line idx))))))

;; so ugly...
(defn neighbours-part2 [point]
  (let [same-plane (map (partial mapv + point)
                        [[-1 1 0 0] [0 1 0 0] [1 1 0 0]
                         [-1 0 0 0] [0 0 0 0] [1 0 0 0]
                         [-1 -1 0 0][0 -1 0 0][1 -1 0 0]])
        above      (map (fn [[x y z w]] [x y (inc z) w]) same-plane)
        below      (map (fn [[x y z w]] [x y (dec z) w]) same-plane)
        forward    (map (fn [[x y z w]] [x y z (inc w)]) same-plane)
        backward   (map (fn [[x y z w]] [x y z (dec w)]) same-plane)
        a-f        (map (fn [[x y z w]] [x y (inc z) (inc w)]) same-plane)
        b-f        (map (fn [[x y z w]] [x y (dec z) (dec w)]) same-plane)
        a-b        (map (fn [[x y z w]] [x y (inc z) (dec w)]) same-plane)
        b-b        (map (fn [[x y z w]] [x y (dec z) (inc w)]) same-plane)]
    (concat (remove #{point} same-plane)
            above below forward backward a-f b-f a-b b-b)))

(defn keep-active?-2 [active-point active]
  (let [nbrs (neighbours-part2 active-point)
        active-neighbours (filter active nbrs)]
    (or (= 3 (count active-neighbours))
        (= 2 (count active-neighbours)))))

(defn make-active?-2 [point active]
  (let [nbrs (neighbours-part2 point)
        active-neighbours (filter active nbrs)]
    (= 3 (count active-neighbours))))

(defn cube-cycle-2 [active]
  (let [neighbours (reduce concat (map neighbours-part2 active))
        keep-act (filter #(keep-active?-2 % active) active)
        make-act (filter #(make-active?-2 % active) neighbours)]
    (into #{} (concat keep-act make-act))))

(defn part-2 [input]
  (let [active (parse-input-2 input)]
    (->> active
         (iterate cube-cycle-2)
         (take 7)
         last
         count)))

(comment (part-2 sample))

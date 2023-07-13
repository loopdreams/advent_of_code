(ns day02-opcode.core
  (:require [clojure.string :as str]))

(def sample "1,9,10,3,2,3,11,0,99,30,40,50")
(def input  (slurp  "input.txt"))

(defn parse-input [input]
  (->> (str/split input #",")
       (map parse-long)))

(defn opcode-run [ints]
  (loop [[a b c d & rest] ints
         state (->> ints (map-indexed vector) (into {}))]
    (when a
      (case a
        99 (vals (sort state))
        1 (recur rest (assoc state d (+ (get state b) (get state c))))
        2 (recur rest (assoc state d (* (get state b) (get state c))))))))

(defn part-1 [input]
  (let [nums (parse-input input)
        edits (concat (into [(first nums)] [12 2]) (drop 3 nums))]
    (first (opcode-run edits))))

(comment
  (println (part-1 input)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
;;
(def target 19690720)

(defn edit-list [list [n1 n2]]
  (concat (into [(first list)] [n1 n2]) (drop 3 list)))

(defn all-pairs []
  (for [i (range 0 100)
        j (range 0 100)]
    [i j]))

(defn find-edit-pair [input]
  (let [nums (parse-input input)]
    (loop [[p & possible] (all-pairs)]
      (let [e-list (edit-list nums p)]
        (if (= (first (opcode-run e-list))
               target)
          p
          (recur possible))))))

(defn part-2 [input]
  (let [[noun verb] (->> input
                         find-edit-pair)]
    (->> noun (* 100) (+ verb))))

(comment (part-2 input))


(ns day08-haunted-wasteland.core
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))
            

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))
(def sample2 (slurp "sample2.txt"))

(defn dirs->keywords [dirs]
  (for [d (seq dirs)]
    (case d
      \L :left
      \R :right)))

(defn parse-input [input]
  (let [[dirs nodes] (str/split input #"\n\n")]
    [(dirs->keywords dirs)
     (into {}
           (for [line (str/split-lines nodes)
                 :let [[k l r] (re-seq #"\w\w\w" line)]]
             [k {:left l
                 :right r}]))]))

(defn part-1 [input]
   (let [[dirs nodes] (parse-input input)]
     (loop [[d & ds] dirs
            pos "AAA"
            step 0]
       (if (= pos "ZZZ") step
           (if d
             (recur ds
                    (d (nodes pos))
                    (inc step))
             (recur dirs pos step))))))

(comment
  (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn part-2 [input]
  (let [[dirs nodes] (parse-input input)
        start-pos (filter #(= (last %) \A) (keys nodes))]
    (for [p start-pos]
      (loop [[d & ds] dirs
             pos p
             step 0]
        (if (= \Z (last pos)) step
            (if d
              (recur ds
                     (d (nodes pos))
                     (inc step))
              (recur dirs pos step)))))))
             
(comment
  (time
   (println
    (->>
     (part-2 input)
     (reduce math/lcm)))))

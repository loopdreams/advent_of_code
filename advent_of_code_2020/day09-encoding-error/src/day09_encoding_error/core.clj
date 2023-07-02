(ns day09-encoding-error.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (->> input str/split-lines (map #(Integer/parseInt %))))

(defn sum-pairs [nums]
  (let [lim (count nums)]
    (loop [n (into [] nums)
           counter 0
           summed-pairs #{}]
      (if (<= counter lim)
        (recur (into [(peek n)] (pop n)) (inc counter)
               (into summed-pairs (map #(+ % (peek n)) (pop n))))
        summed-pairs))))

(defn find-invalid [nums len]
  (if (empty? nums) "All Valid"
      (let [summed (sum-pairs (take len nums))
            candidate (first (drop len nums))]
        (if (summed candidate) (recur (rest nums) len)
            candidate))))

(comment
  (find-invalid (parse-input input) 25)
  (find-invalid (parse-input sample) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
;;

(defn encryption-weakness
  ([target-val nums] (encryption-weakness target-val nums 2))
  ([target-val nums end]
   (let [val (reduce + (take end nums))]
     (cond
       (= val target-val) (take end nums)
       (< val target-val) (recur target-val nums (inc end))
       :else (recur target-val (rest nums) 2)))))

(defn find-encryption-weakness [input len]
  (let [nums (parse-input input)
        target (find-invalid nums len)
        weakness-range (encryption-weakness target nums)
        lowest (first (sort weakness-range))
        highest (first (reverse (sort weakness-range)))]
    (+ lowest highest)))
    
(comment
  (find-encryption-weakness sample 5)
  (find-encryption-weakness input 25))

(ns day12-hot-springs.core
  (:require
   [clojure.math.combinatorics :as c]
   [clojure.string :as str]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn parse-input [input]
  (reduce (fn [result line]
            (conj result
                  (let [[springs info] (str/split line #" ")
                        info (map parse-long (str/split info #","))]
                    [springs info])))
          []
          (str/split-lines input)))

(defn aligned? [s info]
  (= (map count (remove empty? (str/split s #"\.")))
     info))

(defn replace-with-combination [springs comb]
  (loop [[c & cs :as combinations] comb
         [sp & sps] (seq springs)
         result []]
    (if-not sp (apply str result)
            (if (= sp \?)
              (recur cs sps (conj result c))
              (recur combinations sps (conj result sp))))))

(defn all-potential-combos [s]
  (let [candidates (re-seq #"\?" s)]
    (for [combination (c/selections [\. \#] (count candidates))]
      (replace-with-combination s combination))))


(defn valid-combos [[springs info]]
  (let [all-combos (all-potential-combos springs)]
    (count
     (filter #(aligned? % info) all-combos))))

(defn part-1 [input]
  (let [lines (parse-input input)]
    (reduce (fn [result line]
              (+ result (valid-combos line)))
            0
            lines)))

(part-1 input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn unfold [lines]
  (for [l lines
        :let [[springs info] l
              springs (str/join \? (repeat 5 springs))
              info (apply concat (repeat 5 info))]]
    [springs info]))


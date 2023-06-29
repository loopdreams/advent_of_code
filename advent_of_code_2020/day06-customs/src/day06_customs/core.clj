(ns day06-customs.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (->> input
       str/split-lines
       (partition-by #(= % ""))
       (remove #(= (first %) ""))))

(defn count-form [form]
  (->> form seq set count))

(defn sum-uniques [input]
  (let [forms (map str/join (parse-input input))]
    (reduce + (map count-form forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn count-uniques-2 [form]
  (let [person-uniques (map set form)]
    (count (apply set/intersection person-uniques))))

(defn sum-uniques-2 [input]
  (let [forms (parse-input input)]
    (reduce + (map count-uniques-2 forms))))

(ns day01-rocket-equation.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (parse-long line)))

(defn sum-fuel-requirements [modules]
  (apply + (for [m modules]
             (- (int (/ m 3)) 2))))

(comment
  (sum-fuel-requirements (parse-input input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(take-while #(> % 0) [5 4 3 2 1 0 -1 -2 -3])

(defn sum-refined-fuel-requirements [modules]
  (apply + (for [m modules
                 :let [fuels (->> m
                                  (iterate #(- (int (/ % 3)) 2))
                                  (take-while #(> % 0))
                                  rest)]]
             (apply + fuels))))

(println (sum-refined-fuel-requirements (parse-input input)))
(sum-refined-fuel-requirements [1969])

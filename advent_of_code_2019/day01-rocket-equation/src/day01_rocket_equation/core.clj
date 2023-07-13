(ns day01-rocket-equation.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (parse-long line)))

(defn sum-fuel-requirements [modules]
  (reduce (fn [acc module]
            (+ acc
               (- (int (/ module 3)) 2)))
          0
          modules))

(comment
  (sum-fuel-requirements (parse-input input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn sum-refined-fuel-requirements [modules]
  (reduce (fn [acc module]
            (+ acc
               (->> module
                    (iterate #(- (int (/ % 3)) 2))
                    (take-while #(> % 0))
                    rest
                    (apply +))))
          0
          modules))
                    
(comment
  (sum-refined-fuel-requirements (parse-input input)))

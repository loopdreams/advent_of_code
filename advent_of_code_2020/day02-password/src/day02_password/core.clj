(ns day02-password.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map  #(str/split % #"[: ]"))
       (map #(remove #{""} %))))

(defn check-password [[times char password]]
  (let [[start end] (map #(Integer/parseInt %) (str/split times #"-"))
        r (set (range start (inc end)))
        c (count (re-seq (re-pattern char) password))]
    (r c)))

(defn check-all-passwords [input]
  (let [parsed (parse-input input)]
    (->> parsed
         (filter check-password)
         (count))))

(comment (check-all-passwords input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn check-password-part2 [[times char password]]
  (let [[pos1 pos2] (map #(Integer/parseInt %) (str/split times #"-"))]
    (not=
      (= char (subs password (dec pos1) pos1)) 
      (= char (subs password (dec pos2) pos2)))))

(defn check-all-passwords-part2 [input]
  (let [parsed (parse-input input)]
    (->> parsed
         (filter check-password-part2)
         (count))))
         
(comment (check-all-passwords-part2 input))

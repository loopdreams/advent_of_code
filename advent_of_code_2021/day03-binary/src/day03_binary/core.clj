(ns day03-binary.core
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(filter #(= %  \1) (map #(nth % 0) (parse-input (slurp "sample.txt"))))

(defn count-fn [type list]
  (count (filter #(= % type) list)))

(defn rates [lines type]
    (for [i (range 0 (count (first lines)))
          :let [nums (map #(nth % i) lines)
                ones (count-fn \1 nums)
                zeros (count-fn \0 nums)]]
      (case type
        :gamma (if (< ones zeros) "0" "1")
        :epsilon (if (< ones zeros) "1" "0"))))

(defn power-consumption [input]
  (let [lines  (parse-input input)
        g-rate (Long/parseLong (str/join (rates lines :gamma)) 2)
        e-rate (Long/parseLong (str/join (rates lines :epsilon)) 2)]
    (* g-rate e-rate)))


(comment
  (println (power-consumption (slurp "input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn line-type [lines pos type]
  (let [nums (map #(nth % pos) lines)
        ones (count-fn \1 nums)
        zeros (count-fn \0 nums)]
      (case type
        :o2  (cond
               (= ones zeros) "1"
               (< ones zeros) "0"
               :else "1") 
        :co2 (cond
               (= ones zeros) "0"
               (< ones zeros) "1"
               :else "0"))))

(defn -filter [lines type]
  (let [limit (count (first lines))]
    (loop [keep lines
           pos 0]
      (if (= pos (inc limit)) (println "Not Found")
          (if (= 1 (count keep)) keep
              (let [key (line-type keep pos type)]
                (recur
                 (filter #(= (str (nth % pos)) key)
                         keep)
                 (inc pos))))))))

(defn life-support-rating [input]
  (let [lines (parse-input input)
        o2-rating  (Long/parseLong (first (-filter lines :o2)) 2)
        co2-rating (Long/parseLong (first (-filter lines :co2)) 2)]
    (* o2-rating co2-rating)))


(comment
  (println (life-support-rating (slurp "input.txt"))))

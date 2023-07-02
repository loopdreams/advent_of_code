(ns day07-luggage.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def sample2 (slurp "sample2.txt"))
(def input (slurp "input.txt"))

(defn bag->name [a b] (keyword (str a "-" b)))

(defn parse-instruction-line [line]
  (let [[shade colour _ _ & rest] (str/split line #" ")
        bag-name (bag->name shade colour)]
    (if (= (first rest) "no") [bag-name []]
        (loop [[num shade colour _ & r] rest
               result {}]
          (if num
            (recur r (assoc result
                            (bag->name shade colour)
                            (Integer/parseInt num)))
            [bag-name result])))))

(defn parse-input [input] (map parse-instruction-line (str/split-lines input)))

(defn lookup-contains [bag-type bags]
  (map first (filter (fn [[_ v]] (contains? v bag-type)) bags)))

(defn all-containers [bag-type input]
  (let [bags (parse-input input)
        direct-containers (lookup-contains bag-type bags)]
    (loop [candidates direct-containers
           result []]
      (if (empty? candidates) (->> result set count)
          (recur (flatten (map #(lookup-contains % bags) candidates))
                 (into result candidates))))))
    
(comment (all-containers :shiny-gold input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn parse-input-2 [input] (into {} (map parse-instruction-line (str/split-lines input))))

(defn bags->list [bags]
  (reduce concat (map (fn [[k v]] (repeat v k)) bags)))

(defn bag-counter [bag-type bags]
  (loop [queue [bag-type]
         c 0]
    (if (empty? queue) c
        (let [bags-contained (bags->list ((peek queue) bags))]
          (recur (into (pop queue) bags-contained)
                 (+ c (count bags-contained)))))))

(comment
  (bag-counter :shiny-gold (parse-input-2 input)))

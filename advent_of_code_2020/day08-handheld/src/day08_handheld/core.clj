(ns day08-handheld.core
  (:require [clojure.string :as str]))


(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (for [line (->> input str/split-lines)
        :let [[instruction amount] (str/split line #" ")]]
    [(keyword instruction)
     (Integer/parseInt amount)]))

(defn count-to-stoppoint [input]
  (let [points (parse-input input)]
    (loop [visited-indexes  #{}
           acc      0
           location 0]
        (if (visited-indexes location) acc
            (let [[command amount] (nth points location)
                  v (conj visited-indexes location)]
              (case command
                :nop (recur v acc (inc location))
                :acc (recur v (+ acc amount) (inc location))
                :jmp (recur v acc (+ location amount))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn locations-of-nopjmp [points]
  (into [] (remove nil? (map (fn [[idx [key _]]]
                               (when (or (= key :nop)
                                         (= key :jmp)) idx))
                             (map-indexed vector points)))))


;; brute  force approach...

(defn fix-error-and-count [points change-queue]
  (let [target (count points)
        index-to-change (peek change-queue)]
    (loop [visited-indexes  #{}
           acc      0
           location 0]
      (if (= location target) acc
          (if (visited-indexes location)
            (count-to-fixed-program points (pop change-queue))
            (let [[com amount] (nth points location)
                  command (if (= location index-to-change)
                            (if (= com :nop) :jmp :nop)
                            com)
                  v (conj visited-indexes location)]
              (case command
                :nop (recur v acc (inc location))
                :acc (recur v (+ acc amount) (inc location))
                :jmp (recur v acc (+ location amount)))))))))

(defn part-2 [input]
  (let [points (parse-input input)
        change-candidates (locations-of-nopjmp points)]
    (fix-error-and-count points change-candidates)))

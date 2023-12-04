(ns day04-scratchcards.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn parse-num-str [str]
  (->> (str/split (str/trim str) #" ")
       (map parse-long)
       (remove nil?)))

(defn parse-input [input]
  (into {}
        (for [line (str/split-lines input)
              :let [[_ card-no winners my-nums] (str/split line #"Card|:|\|")
                    card-no (parse-long (str/replace card-no #" " ""))
                    [winners my-nums] (map parse-num-str [winners my-nums])]]
          [card-no
           [winners my-nums]])))


(defn calculate-winner-number [[winners my-nums]]
  (let [matches (count (set/intersection (set winners) (set my-nums)))]
    (if (pos? matches)
      (nth (iterate (partial * 2) 1) (dec matches))
      0)))

(defn part-1 [input]
  (let [m (vals (parse-input input))]
    (for [entry m]
      (calculate-winner-number entry))))

(comment
  (apply + (part-1 input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn number-matches [[winners my-nums]]
  (count (set/intersection (set winners) (set my-nums))))

(defn assign-scores [input-m]
  (let [ks (keys input-m)]
    (reduce (fn [result k]
              (let [score (number-matches (input-m k))]
                (-> result
                    (assoc-in [k :score] score)
                    (assoc-in [k :copies] 1))))
            {}
            ks)))

(defn update-copies-entry [k score input-m]
  (let [update-list (map inc (range k (+ k score)))]
    (reduce (fn [result to-update]
              (update-in result [to-update :copies] inc))
            input-m
            update-list)))

(defn part-2 [input]
  (let [scores (assign-scores (parse-input input))
        ks  (sort (keys scores))]
    (map (comp :copies val)
         (reduce (fn [mp k]
                   (let [score (:score (mp k))
                         itr   (:copies (mp k))]
                     (nth (iterate (partial update-copies-entry k score) mp) itr)))
                 scores
                 ks))))

(comment
  (apply + (part-2 input)))

(ns day05-boarding-pass.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input]
  (let [lines (->> input (str/split-lines))
        keywordize (fn [x] (->> x
                                seq
                                (map str)
                                (map keyword)
                                (split-at 7)))]
    (map keywordize lines)))

(defn splits [[start end] key]
  (let [split (/ (- end start) 2)]
    (case key
      :F [start (- end split)]
      :B [(+ start split) end]
      :L (splits [start end] :F)
      :R (splits [start end] :B))))

(defn find-seat-id [[rows cols]]
  (let [[row _] (reduce splits [0 128] rows)
        [col _] (reduce splits [0 8] cols)]
    (+ (* row 8) col)))

(defn find-all-seats [seats] (map find-seat-id seats))

(defn find-highest-seat [input]
  (->> input
       parse-input
       find-all-seats
       sort
       last))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn my-seat [input]
  (let [occupied (->> input
                      parse-input
                      find-all-seats
                      sort)
        last (last occupied)
        first (first occupied)]
    (set/difference (set (range first last))
                    (set occupied))))

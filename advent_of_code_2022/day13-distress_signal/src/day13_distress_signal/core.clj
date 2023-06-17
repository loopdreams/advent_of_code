(ns day13-distress-signal.core
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]))


(defn parse-input [input]
  (loop [[left right _ & rest] (-> input
                                   (str/split-lines))
         processed {}
         count 1]
    (if left
      (let [entry {:left (edn/read-string left)
                   :right (edn/read-string right)
                   :index count}
            key (keyword (str count))]
        (recur rest
               (assoc-in processed [key] entry)
               (inc count)))
      processed)))

(declare compare-lists)

(defn match-types [x1 y1]
  (cond
    (and (sequential? x1) (not (sequential? y1))) [x1 (vector y1)]
    (and (sequential? y1) (not (sequential? x1))) [(vector x1) y1]
    :else [x1 y1]))

(defn empty-lists [x y]
    (cond
      (nil? x) true
      (nil? y) false
      :else (empty-lists (first x) (first y))))

(defn inner-lists [x1 y1]
  (let [[x1-seq y1-seq] (match-types x1 y1)]
    (cond
      (and (empty? (flatten x1-seq))
           (empty? (flatten y1-seq)))
      (empty-lists x1-seq y1-seq)
      :else (compare-lists x1-seq y1-seq))))

(defn compare-lists [p1 p2]
  (let [[x & xs] p1
        [y & ys] p2]
    (cond
      (nil? x)              true
      (nil? y)              false
      (= x y)               (compare-lists xs ys)
      (or (sequential? x)
          (sequential? y)) (inner-lists x y)
      (< x y)              true
      :else                false)))

(defn count-valid [input]
  (let [parsed (parse-input input)]
    (loop [[entry & es] (keys parsed)
           valid-indexes []]
      (if entry
        (let [{left  :left
               right :right
               i     :index} (parsed entry)]
          (if (compare-lists left right)
            (recur es (conj valid-indexes i))
            (recur es valid-indexes)))
        (apply + valid-indexes)))))


(comment
  (count-valid (slurp "input.txt"))
  (count-valid (slurp "sample.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(defn sort-pairs [a b]
  (if (compare-lists a b) -1 1))

(defn decoder-index [input]
  (let [in (->> input
                (str/split-lines)
                (remove str/blank?)
                (map edn/read-string)
                (concat dividers))
        sorted (sort #(sort-pairs %1 %2)
                     (sort #(sort-pairs %2 %1) in))
        x (inc (.indexOf sorted (first dividers)))
        y (inc (.indexOf sorted (second dividers)))]
    (* x y)))

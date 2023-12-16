(ns day13-point-of-incidence.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn parse-input [input]
  (map str/split-lines
       (str/split input #"\n\n")))

(defn check-row-reflections [rows]
  (let [reflect?
        (->>
         (for [r (range 0 (dec (count rows)))]
           [r
            (loop [offset 0
                   result nil]
              (if (and (<= offset r)
                       (< (+ r offset) (dec (count rows))))
                (let [l (nth rows (- r offset))
                      r (nth rows (+ (inc r) offset))]
                  (when (= l r)
                    (recur
                     (inc offset)
                     ((fnil inc 0) result))))
                result))])
         (remove #(nil? (second %))))]
    (when (seq reflect?)
      (->> reflect?
           (sort-by second)
           last
           first
           inc))))

(defn check-vertical-reflections [grid]
  (check-row-reflections
   (for [i (range (count (first grid)))]
     (map #(nth % i) grid))))

(defn part-1 [input]
  (let [grids (parse-input input)]
    (reduce (fn [result grid]
              (let [h (check-row-reflections grid)]
                (if h (+ result (*  h 100))
                    (+ result (check-vertical-reflections grid)))))
                                 
            0
            grids)))


(comment
  (part-1 input))

;; 35232

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn swap-symbol [grid x y]
  (let [replacement (if (= \# (nth (nth grid y) x)) "." "#")
        new-row (replace-at (nth grid y) x replacement)]
    (concat
     (take (dec y) grid)
     [new-row]
     (drop (inc y) grid))))


(defn find-smudge [grid]
  (loop [x 0
         y 0]
    (when (< y (count grid))
      (let [candidate (swap-symbol grid x y)
            h (check-row-reflections candidate)]
        (if h [:horizontal [x y] h]
            (let [v (check-vertical-reflections candidate)]
              (if v [:vertical [x y] v]
                  (recur
                   (if (< x (count (first grid)))  (inc x) 0)
                   (inc y)))))))))

(find-smudge (second (parse-input sample)))

(check-row-reflections (swap-symbol (second (parse-input sample)) 0 0))

(ta -1 [1 2 3])

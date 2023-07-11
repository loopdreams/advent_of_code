(ns day13-folds.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (let [[points folds] (str/split input #"\n\n")

        coords          (into #{}
                              (for [p    (str/split-lines points)
                                    :let [[x y] (str/split p #",")]]
                                (into [] (map parse-long [x y]))))

        commands        (for [f    (str/split-lines folds)
                              :let [[_ dir line] (re-find #"(y|x)=(\d+)" f)]]
                          {:direction (keyword dir) :line (parse-long line)})]
                   
        
    [coords commands]))

(defn draw-map [coords]
  (let [width (->> coords (map first) sort last inc)
        height (->> coords (map second) sort last inc)]
    (for [i (range 0 height)]
      (for [j (range 0 width)]
        (if (coords [j i])
          "#" ".")))))

(defn print-map [m output]
  (spit output (str/join "\n" (map str/join m))))

(defn merge-line [to-line from-line]
  (map (fn merge [x y]
         (if (or (= x "#") (= y "#")) "#" "."))
       to-line
       from-line))

(defn rotate-paper [m]
  (loop [width (count (first m))
         ms m
         rotated []]
    (if (= width 0) rotated
        (recur (dec width)
               (map rest ms)
               (conj rotated (map first ms))))))


(defn count-points [mp]
  (let [all-points (flatten mp)]
    (->> all-points
         (filter #{"#"})
         count)))

(defn fold-up [m fold-line]
  (let [top-half (take fold-line m)
        bottom-half (drop fold-line m)]
    (map merge-line top-half (reverse bottom-half))))

(defn fold-right [m fold-line]
  (rotate-paper (fold-up (rotate-paper m) fold-line)))

(defn fold [m {:keys [direction line]}]
  (case direction
    :x (fold-right m line)
    :y (fold-up m line)))

(defn make-folds [mp commands]
  (loop [[c & cs] commands
         m mp]
    (if c (recur cs (fold m c))
        m)))

(defn count-after-1-fold [input]
  (let [[coords commands] (parse-input input)
        mp (draw-map coords)]
    (->> commands
         first
         list
         (make-folds mp)
         count-points)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn do-all-folds [input]
  (let [[coords commands] (parse-input input)]
    (print-map (make-folds (draw-map coords) commands) "answer.txt")))

(comment (do-all-folds input))

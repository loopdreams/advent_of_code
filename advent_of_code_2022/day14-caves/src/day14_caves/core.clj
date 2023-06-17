(ns day14-caves.core
  (:require [clojure.string :as str]))

(def starting-position [500 0])

(defn str->coords [str]
  (let [[x y] (map #(Integer/parseInt %)
                   (str/split str #","))]
    [x y]))

(defn parse-input [input]
  (loop [[line & lines] (str/split-lines input)
         transformed []]
    (if line
      (recur lines (conj transformed (->> (str/split line #" -> ")
                                          (map str->coords)
                                          (into []))))
      transformed)))

(defn linepoints [[x1 y1] [x2 y2]]
  (let [expand (fn [a b y]
                 (let [[start end] (if (> a b) [b a] [a b])]
                   (for [x (range start (inc end))]
                     [x y])))]
    (if (= x1 x2)
      (map #(into [] (reverse %)) (expand y1 y2 x1))
      (expand x1 x2 y1))))

(defn draw-line [coords]
  (loop [[a b & points] coords
         fill []]
    (if b
      (recur (concat [b] points)
             (concat fill (linepoints a b)))
      fill)))

(defn fill-all-rocks [input]
  (loop [[line & lines] (parse-input input)
         walls []]
    (if line
      (recur lines (concat walls (draw-line line)))
      (set walls))))

(defn collision? [coords walls]
  (walls coords))

(defn wall-collision?
  "For part 2, since 'collision?' look up above was too slow, this one uses a map instead"
  [[x y] walls]
  (let [lookup (keyword (str x y))]
    (lookup walls)))

(defn sand-move [walls]
  (let [floor (->> walls
                   (map second)
                   (sort)
                   (reverse)
                   (first))]
    (loop [[x y]     starting-position
           direction :down
           walls     walls
           count     1]
      (if (> y floor) (dec count)
          (case direction
            :down (if (collision?[x (inc y)] walls)
                    (recur [x y] :left walls count)
                    (recur [x (inc y)] :down walls count))
            :left (if (collision? [(dec x) (inc y)] walls)
                    (recur [x y] :right walls count)
                    (recur [(dec x) (inc y)] :down walls count))
            :right (if (collision? [(inc x) (inc y)] walls)
                     (recur starting-position :down
                            (conj walls [x y])
                            (inc count))
                     (recur [(inc x) (inc y)] :down walls count)))))))

(comment
  (sand-move (fill-all-rocks (slurp "input.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
;;

(defn floor-depth [walls]
  (->> walls
       (map second)
       (sort)
       (reverse)
       (first)
       (+ 2)))


(defn part2-1
  "does not complete"
  [rocks]
  (let [floor (floor-depth rocks)]
    (loop [[x y] starting-position
           sand #{}
           c 600000]
      (if (zero? c) sand
          (if (collision? starting-position sand) (do (println (count sand)) sand)
              (let [test (fn [[x y]] (not (or (collision? [x y] sand)
                                              (collision? [x y] rocks)
                                              (= y floor))))
                    down    [x (inc y)]
                    left    [(dec x) (inc y)]
                    right   [(inc x) (inc y)]]
                (cond
                  (test down) (recur down sand  (dec c))
                  (test left) (recur left sand  (dec c))
                  (test right) (recur right sand  (dec c))
                  :else (recur starting-position (conj sand [x y]) (dec c)))))))))


(defn part2-2 [rocks]
  (let [floor (floor-depth rocks)
        rmap (reduce (fn [acc [x y]] (assoc acc (keyword (str x y)) [x y]))
                     {}
                     rocks)]
    (loop [sand #{}
           [x y] starting-position
           c 4000000]
      (if (zero? c) (do (println "Didn't complete " ) nil)
          (if (collision? starting-position sand) (do (println (count sand)) sand) ;on my computer, emacs crashes sometimes when trying to return 'sand' in widow. But I am returning it here for the 'draw map' functions below
              (let [test    (fn [[x y]] (not (or (collision? [x y] sand)
                                                 (wall-collision? [x y] rmap)
                                                 (= y floor))))
                    down    [x (inc y)]
                    left    [(dec x) (inc y)]
                    right   [(inc x) (inc y)]]
                (cond
                  (test down) (recur sand down  (dec c))
                  (test left) (recur sand left  (dec c))
                  (test right) (recur sand right  (dec c))
                  :else (recur (conj sand [x y]) starting-position (dec c)))))))))



(comment
  (time (part2-1 sample-rocks))
  (time (part2-2 rocks)))


(def rocks (fill-all-rocks (slurp "input.txt")))
(def sample-rocks (fill-all-rocks (slurp "sample.txt")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visualise map
(defn draw-map-line [start end y rocks sand]
  (loop [x start
         line []]
    (if (= x end) line
        (recur
         (inc x)
         (if (collision? [x y] rocks)
           (conj line "#")
           (if (collision? [x y] sand)
             (conj line "*")
             (conj line ".")))))))

(defn draw-map [rocks left right]
  (let [ceiling 0
        sand (part2-2 rocks)
        floor (floor-depth rocks)]
    (loop [y ceiling
           lines []]
      (if (= y floor) (str/join "\n" (map str/join lines))
          (recur
           (inc y)
           (conj lines
                 (draw-map-line left right y rocks sand)))))))

(comment
  (spit "full-map.txt" (draw-map rocks 450 550)))

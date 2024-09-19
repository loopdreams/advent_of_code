(ns day14-disk-defrag.core
  (:require
   [clojure.string :as str]))

;; From Day 10:

(defn swap-els [els start r]
  (let [indxs (map #(mod % (count els)) (range start (+ start r)))]
    (loop [[i & idx] indxs
           [d & destinations] (reverse indxs)
           new-list (into [] els)]
      (if-not i new-list
              (recur
               idx
               destinations
               (assoc new-list d (nth els i)))))))

(defn str->ascii [str]
  (concat
   (map (fn [s] (int s)) (seq str))
   [17 31 73 47 23]))

(defn tie-knots-part-2 [els lengths start skip-size]
  (loop [[l & ls] lengths
         idx start
         els els
         skip-size skip-size]
    (if-not l [els idx skip-size]
            (recur
             ls
             (mod (+ idx l skip-size) 256)
             (swap-els els idx l)
             (inc skip-size)))))

(defn transform-group [g]
  (format "%02x" (apply bit-xor g)))

(defn result->hex [result]
  (let [groups (partition 16 result)]
    (str/join
     (map transform-group groups))))

(defn tie-all-knots [str]
  (let [lengths (str->ascii str)]
    (loop [c 64
           els (range 256)
           idx 0
           skip-size 0]
      (if (> c 0)
        (let [[new-els new-idx new-skip] (tie-knots-part-2 els lengths idx skip-size)]
          (recur (dec c)
                 new-els
                 new-idx
                 new-skip))
        (result->hex els)))))


;; End Day 10

(defn hex->binary [hex-char]
  (case hex-char
    \0 "0000"
    \1 "0001"
    \2 "0010"
    \3 "0011"
    \4 "0100"
    \5 "0101"
    \6 "0110"
    \7 "0111"
    \8 "1000"
    \9 "1001"
    \a "1010"
    \b "1011"
    \c "1100"
    \d "1101"
    \e "1110"
    \f "1111"))

(defn binary-string [hex]
  (->> hex (map hex->binary) str/join))

(def make-grid
  (memoize
   (fn [input]
     (for [i (range 128)]
       (binary-string
        (tie-all-knots (str input "-" i)))))))

(defn count-used [input]
  (reduce (fn [acc row]
            (+ acc ((frequencies row) \1)))
          0
          (make-grid input)))


(comment (count-used "flqrgnkx"))

;; Part-2

(defn neighbours [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0]]))

(defn grid->arrays [grid]
  (into []
        (for [line grid]
          (into [] (seq line)))))


(defn lookup [grid-array [x y]]
  (nth (nth grid-array y) x))

(def coords
  (->>
   (for [y (range 128)]
     (for [x (range 128)]
       [x y]))
   (reduce concat)
   (into #{})))


(defn count-regions [grid-array]
  (loop [to-check            coords
         active-region-queue #{}
         region-count        0]
    (cond
      (and (empty? to-check) (empty? active-region-queue))
      region-count

      (empty? active-region-queue)
      (let [p (first to-check)]
        (if (= \1 (lookup grid-array p))
          (let [nbrs (filter to-check (neighbours p))]
            (recur
             (apply (partial disj to-check) (conj nbrs p))
             (into active-region-queue (filter to-check (neighbours p)))
             (inc region-count)))
          (recur
           (disj to-check p)
           active-region-queue
           region-count)))

      :else
      (let [p (first active-region-queue)]
        (if (= \1 (lookup grid-array p))
          (let [nbrs (filter to-check (neighbours p))]
            (recur
             (apply (partial disj to-check) nbrs)
             (into (disj active-region-queue p) nbrs)
             region-count))
          (recur
           to-check
           (disj active-region-queue p)
           region-count))))))

(comment
  (println
   (-> (make-grid "flqrgnkx")
       (grid->arrays)
       (count-regions))))

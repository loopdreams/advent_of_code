(ns day10-knot-hash.day10-knot-hash
  (:require
   [clojure.string :as str]))

(def sample [3 4 1 5])

(def input [206 63 255 131 65 80 238 157 254 24 133 2 16 0 1 3])

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

(defn tie-knots [n-elements lengths]
  (loop [[l & ls] lengths
         idx 0
         els (range n-elements)
         skip-size 0]
    (if-not l (apply * (take 2 els))
            (recur
             ls
             (mod (+ idx l skip-size) n-elements)
             (swap-els els idx l)
             (inc skip-size)))))

(comment (tie-knots 256 input))

;; part 2

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
  (Integer/toString
   (apply bit-xor g)
   16))

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

(println (tie-all-knots "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3"))

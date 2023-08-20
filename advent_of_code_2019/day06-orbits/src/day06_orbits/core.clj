(ns day06-orbits.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (for [line (str/split-lines input)
        :let [[_ A B] (re-find #"(.*)\)(.*)" line)]]
    [A B]))

(defn map-orbits [data]
  (reduce (fn [m [x y]]
            (update m x conj y))
          {}
          data))

(defn get-path [node orbits]
  (loop [lookup-q (into [] (get orbits node))
         path []]
    (if (empty? lookup-q) path
        (let [target (peek lookup-q)
              paths (get orbits target)]
          (recur (into (pop lookup-q) paths)
                 (conj path target))))))

(defn count-paths [input]
  (let [orbits (->> input parse-input map-orbits)
        ks (keys orbits)]
    (->> ks
         (reduce #(conj %1 (get-path %2 orbits)) [])
         flatten
         count)))


(comment
  (count-paths input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn orbiting-x [node orbits]
  (map key (filter #((set (val %)) node) orbits)))

(defn find-reverse-path [node orbits]
  (loop [n node
         path []]
    (if (empty? n) path
        (recur (first (orbiting-x n orbits))
               (conj path n)))))

(defn count-path-to-destination [n1 n2 input]
  (let [orbits (->> input parse-input map-orbits)
        p1 (find-reverse-path n1 orbits)
        p2 (find-reverse-path n2 orbits)
        intersect (set/intersection (set p1) (set p2))
        p1 (remove intersect p1)
        p2 (remove intersect p2)]
    (- (+ (count p1) (count p2)) 2)))

(comment (count-path-to-destination "YOU" "SAN" sample))

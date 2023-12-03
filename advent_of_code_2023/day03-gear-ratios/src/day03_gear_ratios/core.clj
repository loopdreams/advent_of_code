(ns day03-gear-ratios.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn num-coords [y len idx]
  (for [n (range len)
        :let [x (+ idx n)]]
    [x y]))

(defn parse-line [line y m]
  (loop [[p & parts] (str/split line #"(?!\d)|(?<!\d)")
         result      m
         idx         0]
    (if-not p result
            (cond
              (empty? p)     (recur parts result (inc idx))
              (parse-long p) (let [len (count p)]
                               (recur parts
                                      (update result p
                                              (fnil conj []) (num-coords y len idx))
                                      (+ idx len)))
              :else          (recur parts
                                    (update result p
                                            (fnil conj []) [idx y])
                                    (inc idx))))))

(defn parse-input [input]
  (loop [[l & lines] (str/split-lines input)
         result {}
         y 0]
    (if-not l result
            (recur lines
                   (parse-line l y result)
                   (inc y)))))


(defn touching [coord]
  (map (partial mapv + coord)
       [[-1 -1] [0 -1] [1 -1]
        [-1 0]         [1 0]
        [-1 1]  [0 1]  [1 1]]))

(defn coords-touching [m]
  (let [symbols-at (reduce concat (vals (select-keys m ["*" "$" "#" "+" "=" "/" "&" "@" "%" "-"])))]
    (set (reduce concat (map touching symbols-at)))))

(defn check-touching [vals touching]
  (count
   (remove nil?
           (map #(some touching %) vals))))

(defn part-1 [input]
  (let [points           (parse-input input)
        touching-symbols (coords-touching points)]
    (apply +
           (map (fn [[k vs]]
                  (let [num-touching (check-touching vs touching-symbols)]
                    (if (pos? num-touching)
                      (* (parse-long k) num-touching)
                      0)))
                points))))

(comment
  (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn return-touching-points [p m nums]
  (let [nbrs (touching p)]
    (remove nil?
            (for [n nums
                  :let [candidates (m n)]]
              (when (seq
                     (remove nil? (map #(some (set nbrs) %) candidates)))
                n)))))

(defn nums-touching-gears [input]
  (let [points (parse-input input)
        nums (filter parse-long (keys points))]
    (loop [[g & gs] (points "*")
           result {}]
      (if-not g result
              (let [touching-points (return-touching-points g points nums)]
                (recur gs
                       (update result g (fnil conj []) touching-points)))))))

(defn part-2 [input]
  (let [gears-m (vals (nums-touching-gears input))]
    (apply +
           (remove nil?
                   (map (fn [[v]]
                          (when (= (count v) 2)
                            (let [[x y] (map parse-long v)]
                              (* x y))))
                        gears-m)))))

(comment
  (part-2 input))

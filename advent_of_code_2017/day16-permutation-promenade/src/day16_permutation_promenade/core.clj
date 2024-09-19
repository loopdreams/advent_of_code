(ns day16-permutation-promenade.core
  (:require
   [clojure.string :as str]))

(def input (slurp "D16.txt"))

(def char-idx (into [] (range 16)))

(def spin
  (memoize
   (fn [ps n]
     (reduce (fn [nps idx]
               (assoc nps idx (nth ps (mod (- idx n) 16))))
             []
             char-idx))))

(def swapping
  (memoize
   (fn [ps p1 p2 v1 v2]
     (-> ps
         (assoc p1 v2)
         (assoc p2 v1)))))

(defn exchange [ps p1 p2]
  (let [v1 (nth ps p1)
        v2 (nth ps p2)]
    (swapping ps p1 p2 v1 v2)))

(defn partner [ps v1 v2]
  (let [p1 (.indexOf ps v1)
        p2 (.indexOf ps v2)]
    (swapping ps p1 p2 v1 v2)))

(defn parse-input [input]
  (let [vals (str/split (first (str/split-lines input)) #",")]
    (for [v vals]
      (case (first v)
        \x (let [[p1 p2] (map parse-long (re-seq #"\d+" v))]
             [:exchange p1 p2])
        \p (let [[v1 v2] (map (fn [x] (int (.charAt x 0))) (str/split (subs v 1) #"/"))]
             [:partner v1 v2])
        \s [:spin (parse-long (subs v 1))]))))

(defn init-dancers [] (into [] (range 97 113)))

(def order-programs
  (memoize
   (fn [steps ps]
     (reduce (fn [ps [type a b]]
               (case type
                 :exchange (exchange ps a b)
                 :partner  (partner ps a b)
                 :spin     (spin ps a)))
             ps
             steps))))

(comment
  (-> (parse-input input)
      (order-programs (init-dancers))
      ((partial map char))
      (str/join)
      (println)))

;; Part 2

(defn ntimes-dances [steps ps n]
  (loop [n n
         positions ps]
    (if (zero? n) positions
        (recur (dec n) (order-programs steps positions)))))

(defn ntimes-dances-pattern [steps start-ps]
  (loop [n 1
         positions (order-programs steps start-ps)]
    (if (= positions start-ps)
      (ntimes-dances steps start-ps (mod 1000000000 n))
      (recur (inc n) (order-programs steps positions)))))

(comment
  (-> (parse-input input)
      (ntimes-dances-pattern (init-dancers))
      ((partial map char))
      str/join
      println))


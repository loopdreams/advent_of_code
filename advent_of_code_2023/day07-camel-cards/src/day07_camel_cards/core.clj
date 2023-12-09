(ns day07-camel-cards.core
  (:require [clojure.string :as str]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn parse-input [input]
  (for [line (str/split-lines input)
        :let [[hand bid] (str/split line #" ")
              bid (parse-long bid)]]
    [hand bid]))

(defn set-shape [h]
  (->> h
       sort
       (partition-by identity)
       (map count)
       sort
       (into [])))

(defn determine-hand [hand]
  (let [shape (set-shape hand)]
    (cond
      (apply = (seq hand)) :5-of-kind
      (= 2 (count shape)) (if (= shape [2 3]) :full-house :4-of-kind)
      (= 3 (count shape)) (if (= (last shape) 3) :3-of-kind :2-pair)
      (= 4 (count shape)) :pair
      :else :high-card)))

(defn group-by-type [hands]
  (reduce (fn [m [v h]]
            (update m v (fnil conj []) h))
          {}
          (for [h hands
                :let [type (determine-hand (first h))]]
            [type h])))

(defn card->val [card]
  ((zipmap [\T \J \Q \K \A] (range 10 16))
   card
   (parse-long (str card))))

(defn sort-nums [[n1 & n1s] [n2 & n2s]]
  (if-not n1
    true
    (if (= n1 n2) (sort-nums n1s n2s)
        (if (< n1 n2) false true))))

(def hand-order [:5-of-kind :4-of-kind :full-house :3-of-kind :2-pair :pair :high-card])

(defn sort-two-hands [[h1 _] [h2 _]]
  (let [h1 (map card->val h1)
        h2 (map card->val h2)]
    (sort-nums h1 h2)))

(defn sort-hand-groups [m]
  (reduce concat
          (for [k hand-order
                :let [vs (k m)]
                :when vs]
            (sort sort-two-hands vs))))

(defn part-1 [input]
  (let [groups (group-by-type (parse-input input))
        sorted (sort-hand-groups groups)]
    (for [s (map-indexed vector (reverse sorted))
          :let [[r [_ bid]] s
                rank (inc r)]]
      (* rank bid))))

(comment
  (println
   (->> (part-1 input)
        (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn cards->vals [cs]
  (->> (map card->val cs)
       sort
       (partition-by identity)
       (sort-by count)
       reverse
       flatten))

(defn restructure-hand [hand-vals-ordered]
  (if (every? #(= % 11) hand-vals-ordered) hand-vals-ordered
      (let [num-js     (count (filter #(= % 11) hand-vals-ordered))
            remove-js  (remove #(= % 11) hand-vals-ordered)
            hand-shape (partition-by identity remove-js)
            target-val (first remove-js)]
        (reduce concat (into (first hand-shape) (repeat num-js target-val))
                (rest hand-shape)))))

(defn part-2-group-by-type [hands]
  (reduce (fn [m [v h]]
            (update m v (fnil conj []) h))
          {}
          (for [h hands
                :let [type (determine-hand (restructure-hand (cards->vals (first h))))]]
            [type h])))

(defn p2-card->val [card]
  ((zipmap [\J \T \Q \K \A] (conj (remove #{11} (range 10 16)) 1))
   card
   (parse-long (str card))))

(defn p2-sort [[h1 _] [h2 _]]
  (let [h1 (map p2-card->val h1)
        h2 (map p2-card->val h2)]
    (sort-nums h1 h2)))

(defn p2-sort-hand-groups [m]
  (reduce concat
          (for [k hand-order
                :let [vs (k m)]
                :when vs]
            (sort p2-sort vs))))

(defn part2 [input]
  (let [groups (part-2-group-by-type (parse-input input))
        sorted (p2-sort-hand-groups groups)]
    (for [s (map-indexed vector (reverse sorted))
          :let [[r [_ bid]] s
                rank (inc r)]]
      (* rank bid))))

(comment
  (println
   (->> (part2 input)
        (reduce +))))

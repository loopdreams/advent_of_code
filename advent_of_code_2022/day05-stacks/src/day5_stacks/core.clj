(ns day5-stacks.core
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "input.txt")))

(defn find-init-height
  "assuming crates are never numbered..."
  [input]
  (let [nums (map #(re-find #"\d+" %) input)]
    (loop [[n & ns] nums
           c 0]
      (if (nil? n)
        (recur ns (inc c))
        c))))

(defn padding [crate len]
  (let [x (count crate)
        padding (repeat (- len x) " ")]
    (s/join [crate (s/join padding)])))

(defn add-padding [crates]
  (let [len (last (sort (map #(count %) crates)))]
    (map #(padding % len) crates)))

(defn crates->lists [crates]
  (let [total-len (dec (count (last crates)))]
    (loop [start 1
           end 2
           result []]
      (if (<= end total-len)
        (recur
         (+ 4 start)
         (+ 4 end)
         (conj result
               (into [] (remove #(= % " ")
                                (map #(subs % start end) crates)))))
        result))))

(defn parse-instructions [instructions]
  (let [nums (map #(re-seq #"\d+" %)
                  instructions)
        move (map first nums)
        froms (map rest nums)]
    (interleave move froms)))

(defn transform-input [input]
  (let [crate-height (find-init-height input)
        crates (crates->lists (add-padding (take crate-height input)))
        instructions (parse-instructions (drop (+ crate-height 2) input))]
    [crates instructions]))

(defn single-move [crates move from-to]
  (let [m (Integer/parseInt move)
        [from to] (map dec (map #(Integer/parseInt %) from-to))
        items (reverse (take m (nth crates from)))
        new-stack-from (into [] (drop m (nth crates from)))
        new-stack-to (into [] (flatten (concat items (nth crates to))))]
    (assoc
     (assoc crates from new-stack-from)
     to new-stack-to)))

(defn single-move-part2 [crates move from-to]
  (let [m (Integer/parseInt move)
        [from to] (map dec (map #(Integer/parseInt %) from-to))
        items (take m (nth crates from))
        new-stack-from (into [] (drop m (nth crates from)))
        new-stack-to (into [] (flatten (concat items (nth crates to))))]
    (assoc
     (assoc crates from new-stack-from)
     to new-stack-to)))

(defn perform-moves [input]
  (let [[crates instructions] (transform-input input)]
    (loop [[move from-to & rest] instructions
           c crates]
      (if (empty? move) c
          (recur
           rest
           (single-move c move from-to))))))

(defn perform-moves-part2 [input]
  (let [[crates instructions] (transform-input input)]
    (loop [[move from-to & rest] instructions
           c crates]
      (if (empty? move) c
          (recur
           rest
           (single-move-part2 c move from-to))))))

(defn -main
  [& _]
  (let [moves (perform-moves input)
        answer (s/join (map first moves))]
    (println answer)
    answer))

(defn part2 []
  (let [moves (perform-moves-part2 input)
        answer (s/join (map first moves))]
    (println answer)
    answer))

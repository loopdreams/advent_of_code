(ns day11-monkey.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s]))


(def input (->> (slurp "input.txt")
                (s/split-lines)
                (partition-by #(= % ""))
                (remove  #(= (first %) ""))))

(defn process-monkey [monkey]
  (let [name-fn  (fn [string] (->> (re-find #"\d+" string)
                                   (str "m")
                                   (keyword)))
                                  
        name     (name-fn (first monkey))
        items    (->> (second monkey)
                      (re-seq #"\d+")
                      (map #(Integer/parseInt %))
                      (into []))
        operator (->> (nth monkey 2)
                      (re-find #"\+|\*"))
        operand  (if (re-find #"\d+" (nth monkey 2))
                   (Integer/parseInt (re-find #"\d+" (nth monkey 2)))
                   "old")
        op-fun   (fn [x] ((resolve (symbol operator)) x (if (= operand "old") x operand)))
        test     (Integer/parseInt (re-find #"\d+" (nth monkey 3)))
        if-true  (name-fn (nth monkey 4))
        if-false (name-fn (nth monkey 5))
        test-fn  (fn [x] (if (zero? (mod x test)) if-true if-false))
        info-map (zipmap [:items :op :test] [items op-fun test-fn])]
    [name info-map]))

(defn process-input [input]
  (loop [monkeys input
         result {}]
    (if (empty? monkeys) result
        (let [[name info] (process-monkey (first monkeys))]
          (recur
           (rest monkeys)
           (assoc result name info))))))


(defn m-test [monkey value monkeys]
  ((:test (monkey monkeys)) value))

(defn m-operate [monkey item monkeys]
  (let [m (monkey monkeys)]
    ((:op m) item)))


(defn m-throw [monkey value monkeys]
  (let [item (unchecked-int (/ value 3))
        to (m-test monkey item monkeys)]
    (update-in monkeys [to :items] conj item)))

(defn single-monkey-round [monkey state]
  (loop [items (:items (monkey state))
         ms state
         count 0]
    (if (empty? items) [(assoc-in ms [monkey :items] []) count]
        (let [item (m-operate monkey (first items) ms)]
          (recur
           (rest items)
           (m-throw monkey item ms)
           (inc count))))))


(defn monkey-rounds [monkeys rounds]
  (loop [ms monkeys
         monkey (into [] (keys monkeys))
         r (* rounds (count (keys monkeys)))
         items-inspected (zipmap (keys monkeys) (repeat 0))]
    (if (zero? r) items-inspected
        (let [[updated-ms inspected] (single-monkey-round (first monkey) ms)]
          (recur
           updated-ms
           (concat (rest monkey) [(first monkey)])
           (dec r)
           (update-in items-inspected [(first monkey)] + inspected))))))


(defn -main
  [& _]
  (let [most-inspections (->> (monkey-rounds (process-input input) 20)
                              (sort-by val)
                              (reverse)
                              (take 2))
        totals (reduce * (vals most-inspections))]
    (printf "Monkeys who inspected the most were:\n")
    (pprint most-inspections)
    (printf "The level of monkey business was: %d\n" totals)
    totals))

(monkey-rounds (process-input input) 20)

(-main)

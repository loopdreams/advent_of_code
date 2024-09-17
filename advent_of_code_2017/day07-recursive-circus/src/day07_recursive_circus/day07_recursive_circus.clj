(ns day07-recursive-circus.day07-recursive-circus
  (:require
   [clojure.string :as str]))

(def input (slurp "D07.txt"))
(def sample (slurp "sample.txt"))

(defn parse-holding [holding]
  (-> holding
      (str/replace #" " "")
      (str/split #",")))

(defn parse-line [line]
  (let [[name holding] (str/split line #"->")
        val (parse-long (re-find #"\d+" name))
        name (first (str/split line #" \("))]
    (if holding
      [name val (parse-holding holding)]
      [name val])))

(defn input->vecs [input]
  (let [lines (str/split-lines input)]
    (map parse-line lines)))


(defn has-holding? [candidate vecs]
  (filter (fn [[_ _ holdings]] (when holdings (some #{candidate} holdings))) vecs))

(defn find-root
  "Root is the name that has nothing 'holding' it"
  [vecs]
  (loop [[v & vs] vecs]
    (when v
      (let [holding (has-holding? (first v) vecs)]
        (if (empty? holding) (first v)
            (recur vs))))))

(comment
  (find-root (input->vecs input)))


;; Part 2

(def get-value
  (memoize (fn [vecs k]
             (second
              (first
               (filter (fn [[key]] (= key k)) vecs))))))

(def get-holding
  (memoize (fn [vecs k]
             (first
              (filter (fn [[key]] (= key k)) vecs)))))

(defn program-vals [vecs program]
  (let [[_ _ holding] (get-holding vecs program)]
    (if holding
      (into [(get-value vecs program)]
            (map (partial program-vals vecs) holding))
      (get-value vecs program))))


(defn sum-all [lsts]
  (for [l lsts]
    (if (coll? l)
      (apply + (flatten l))
      l)))

(defn is-balanced? [[self & towers]]
  (apply = (sum-all towers)))

(defn re-balancer [[self & rest]]
  (let [sums     (sum-all rest)
        sum-fq   (frequencies sums)
        sum-diff (apply - (keys sum-fq))
        tgt-diff (->> sum-fq (filter #(= (val %) 1)) ffirst)
        tgt-diff (first (nth rest (.indexOf sums tgt-diff)))]
    (- tgt-diff sum-diff)))

(defn part-2 [input]
  (->> (map (comp (partial program-vals input) first) input)
       (filter coll?)
       (remove is-balanced?)
       (sort-by count)
       first
       re-balancer))

(part-2 (input->vecs input))

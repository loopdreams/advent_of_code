(ns day14-polymers.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-first-last-chars [input]
  (let [[start-seq _] (str/split input #"\n\n")]
    [(first start-seq) (last start-seq)]))

(defn parse-input [input]
  (let [[start-seq rules] (str/split input #"\n\n")

        parsed-rules      (into {}
                                (for [r    (str/split-lines rules)
                                      :let [[_ pair middle]
                                            (re-find #"(\w+) -> (\w+)" r)]]
                                  [pair (.charAt middle 0)]))

        start-map         (loop [[a b & rest] start-seq
                                 s-m          {}]
                            (if b
                              (recur (cons b rest)
                                     (update s-m (str a b) (fnil inc 0)))
                              s-m))]
    [start-map parsed-rules]))

;; main fn
(defn update-poly [m rules]
  (reduce (fn [m [k v]]
            (let [mid (get rules k)
                  v1 (str (first k) mid)
                  v2 (str mid (second k))]
              (-> m
                  (update v1 (fnil + 0) v)
                  (update v2 (fnil + 0) v))))
          {}
          m))


(defn halve-vals [m]
  (into {} (for [[k v] m] [k (/ v 2)])))

(defn count-letters [start-letter end-letter m]
  (let [doubled-letters (reduce (fn [result [k v]]
                                  (let [[a b] (seq k)]
                                    (-> result
                                        (update a (fnil + 0) v)
                                        (update b (fnil + 0) v))))
                                {}
                                m)
        doubled-letters (-> doubled-letters
                            (update start-letter dec)
                            (update end-letter dec))]
     (-> doubled-letters
         halve-vals
         (update start-letter inc)
         (update end-letter inc))))


(defn build-polys [input amount]
  (let [[m rules]    (parse-input input)
        [start-letter end-letter]  (parse-first-last-chars input)
        result       (->> m
                          (iterate #(update-poly % rules))
                          (take (inc amount))
                          last
                          (count-letters start-letter end-letter)
                          vals
                          sort)]
    (- (last result) (first result))))

(comment
  (time (println (build-polys input 40))))

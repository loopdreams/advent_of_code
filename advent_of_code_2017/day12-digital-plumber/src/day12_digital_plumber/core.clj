(ns day12-digital-plumber.core
  (:require
   [clojure.string :as str]))

(def input (slurp "D12.txt"))
(def sample (slurp "sample.txt"))

(defn parse-input [input]
  (into {}
        (for [line (str/split-lines input)
              :let [[p1 p2] (str/split line #" <-> ")
                          p1 (parse-long p1)
                          p2s (mapv parse-long (str/split p2 #", "))]]
          [p1 p2s])))


(defn get-group [conns target]
  (loop [queue [target]
         nodes []]
    (if (empty? queue) nodes
        (let [nxt (peek queue)
              n-nodes (conj nodes nxt)
              n-queue (into [] (remove (set n-nodes) (concat (pop queue) (conns nxt))))]
          (recur n-queue
                 n-nodes)))))


(defn part-1 [input]
  (count (get-group (parse-input input) 0)))


(comment
  (part-1 input))

;; Part-2

(defn count-groups [conns]
  (let [nodes (keys conns)]
    (loop [[n & ns] nodes
           groups #{}]
      (if-not n (count groups)
              (recur ns
                     (conj groups (set (get-group conns n))))))))

(comment
  (count-groups (parse-input input)))

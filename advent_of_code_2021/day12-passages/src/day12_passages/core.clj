(ns day12-passages.core
  (:require [clojure.string :as str]))


(def sample (slurp "sample.txt"))

(defn small-cave? [n]
  (= (name n) (str/lower-case (name n))))

(defn all-connections [parsed]
  (reduce (fn [m [k vs]]
            (loop [[v & rest] vs
                   mp m]
              (if v
                (recur rest
                       (update mp v conj k))
                mp)))
          parsed
          parsed))

(defn init-unvisited [all-connections]
  (reduce (fn [m [k vs]]
            (if (small-cave? k)
              (update m k conj :unvisited)
              m))
          all-connections
          all-connections))

(defn parse-input [input]
  (let [routes        (->> input
                           str/split-lines
                           (map #(str/split % #"-")))
        positions (zipmap (->> routes
                               (reduce concat)
                               set
                               (mapv keyword))
                          (repeat #{}))]
    (init-unvisited (all-connections
                     (reduce (fn [m [from to]]
                               (update m (keyword from) conj (keyword to)))
                             positions
                             routes)))))




;; (defn find-path
;;   ([nodes] (find-path nodes :start [] #{}))
;;   ([nodes n path paths]
;;    (let [u-path (conj path n)]
;;      (if
;;        (= n :end) (do (println u-path)
;;                       (recur nodes (peek path) path (conj paths u-path)))
;;        (let [[neighbour & rest] (n nodes)
;;              new-nodes (if (small-cave? n)
;;                          (-> nodes
;;                              (update n conj :visited)
;;                              (update n disj :unvisted))
;;                          nodes)]
;;          (cond
;;            (= neighbour :start) (recur n)))))))

;; (defn find-path [points]
;;   (let [start (filter #(= (first %) :start) points)
;;         search (remove #(= (first %) :start) points)]
;;     (loop [queue (second start)
;;            paths []])))


;; (find-path (parse-input sample))

;; (= (name :e) (str/lower-case (name :e)))

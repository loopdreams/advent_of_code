(ns day07-directory.core
  (:gen-class)
  (:require [clojure.string :as s]))

(def input (s/join " " (s/split-lines (slurp "input.txt"))))


(defn sum-sizes [ls]
  (apply + (map #(Integer/parseInt %)
                (re-seq #"\d+" ls))))

(defn subdir-vals [dirs m]
  (apply +  (map #((keyword %) m) dirs)))

(defn add-subdirs [sizes subdirs]
  (loop [s subdirs
         result sizes]
    (if (empty? s) result
        (let [[subs d & xs] s]
          (if (empty? subs)
            (recur
             xs
             sizes)
            (recur
             xs
             (update-in result [d] +
                        (subdir-vals subs result))))))))
  
(defn map-dirs [input]
  (let [splits (filter #(re-find #"ls" %)
                       (s/split input #"cd "))
        names (map keyword (map str  (map first splits)))
        subdirs (reverse
                 (interleave names
                             (map #(map second (re-seq #" dir (\w+)" %)) splits)))
        sizes (zipmap names
                      (map sum-sizes splits))]
    (add-subdirs sizes subdirs)))


(defn -main
  [& _]
  (let [dirs (map-dirs input)]
    (apply + (vals (into {}
                         (filter
                          (fn [[k v]] (< v 100000)) dirs))))))

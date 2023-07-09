(ns day14-bitmask.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def and-keys {\X 1 \1 \1 \0 \0})
(def or-keys  {\X 0 \1 \1 \0 \0})

(defn bitmask-create [bitmask fn]
  (Long/parseLong
   (->> bitmask
        (map #(fn %))
        (str/join)) 2))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (for [l lines
          :let [[key val] (str/split l #" = ")]]
      (if (= key "mask")
        [:mask [(bitmask-create val or-keys)
                (bitmask-create val and-keys)]]
        [:mem [(parse-long (re-find #"\d+" key))
               (parse-long val)]]))))

(defn memory-sum [input]
  (let [commands (parse-input input)]
    (loop [[c & cs] commands
           masks    nil
           mem      {}]
      (if c
        (if (= (first c) :mask)
          (recur cs (second c) mem)
          (let [[loc val] (second c)
                mem-val (->> val
                             (bit-or (first masks))
                             (bit-and (second masks)))]
            (recur cs masks (assoc mem loc mem-val))))
        (apply + (vals mem))))))

(comment (println (memory-sum input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn parse-input-2 [input]
  (let [lines (str/split-lines input)]
    (for [l lines
          :let [[key val] (str/split l #" = ")]]
      (if (= key "mask")
        [:mask val]
        [:mem [(parse-long (re-find #"\d+" key))(parse-long val)]]))))

(defn int->binstr [int]
  (let [end (Integer/toBinaryString int)
        filled (count end)
        filler (str/join (repeat (- 36 filled) "0"))]
    (str filler end)))

(defn apply-mask [mask val]
  (apply str (map (fn [v m]
                    (cond
                      (= m \X) \X
                      (= m  v) v
                      :else    \1))
                  val mask)))

(defn replace-Xs [string]
  (if (re-find #"X" string)
    (let [x (str/replace-first string \X 1)
          y (str/replace-first string \X 0)]
      (conj [] (replace-Xs x) (replace-Xs y)))
    string))

(defn mem-decoder [input]
  (let [commands (parse-input-2 input)]
    (loop [[c & cs] commands
           mem {}
           mask nil]
      (if c
        (if (= (first c) :mask) (recur cs mem (second c))
            (let [[addr val] (second c)
                  mem-val (int->binstr addr)
                  mask-binaries (->> mem-val
                                 (apply-mask mask)
                                 (replace-Xs)
                                 flatten)
                  mask-ints (map #(Long/parseLong % 2) mask-binaries)]
              (recur cs
                     (reduce #(assoc %1 %2 val) mem mask-ints)
                     mask)))
        (apply + (vals mem))))))

(comment (println (mem-decoder input)))

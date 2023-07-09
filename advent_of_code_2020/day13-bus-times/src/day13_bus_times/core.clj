(ns day13-bus-times.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (let [[timestamp buses] (str/split-lines input)
        bus-list (str/split buses #",")]
    [(Integer/parseInt timestamp)
     (map (fn [x] (if (= "x" x) x
                      (parse-long x)))
          bus-list)]))

;; trying to learn about laziness...
(defn lazy-timetable
  ([iterator] (lazy-timetable iterator 0))
  ([iterator n]
   (lazy-seq (cons n (lazy-timetable iterator (+ n iterator))))))

(defn next-bus [now bus-number]
  [bus-number (nth (lazy-timetable bus-number)
                   (inc (int (/ now bus-number))))])

(defn find-next-bus [input]
 (let [[now b] (parse-input input)
       buses (remove #{"x"} b)]
   (->> buses
        (map #(next-bus now %))
        (map (fn [[n time]] [n (- time now)]))
        (sort-by second)
        first
        (apply *))))

;; simpler solution...
;;
(defn bus-at-t? [t buses]
  (for [b buses
        :when (zero? (mod t b))]
    b))

(defn find-simple-bus [input]
  (let [[now b] (parse-input input)
        buses (remove #{"x"} b)]
    (loop [t now]
      (let [buses-at-t (bus-at-t? t buses)]
        (if (empty? buses-at-t) (recur (inc t))
            (* (first buses-at-t) (- t now)))))))

(comment (time (find-next-bus input))
         (time (find-simple-bus input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
;;

(defn process-input [input]
  (let [[_ bs] (parse-input input)]
    (->> bs
         (map-indexed vector)
         (remove (fn [[idx val]]
                   (= val "x"))))))

(defn find-sync-point [input]
  (let [[b & bs] (process-input input)]
    (loop [t         0
           buses     bs
           step-size (second b)]
      (if (empty? buses) t
          (let [[idx name] (first buses)
                next-t     (loop [t-sub t]
                             (if (not= (mod (+ t-sub idx) name) 0)
                               (recur (+ t-sub step-size))
                               t-sub))]
            (recur next-t (rest buses) (* step-size name)))))))

(comment (println (find-sync-point input)))

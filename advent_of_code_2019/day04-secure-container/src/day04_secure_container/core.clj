(ns day04-secure-container.core)

(def input (range 271973 785962))

(defn has-repeating-digits? [coll]
  (when (= 6 (count coll))
    (> 6  (count (partition-by identity coll)))))

(defn has-increasing-digits? [coll]
  (= (sort coll) coll))

(defn validate-password [nums]
  (let [nums (->> nums str (map (comp read-string str)))]
    (and(has-increasing-digits? nums)
        (has-repeating-digits? nums))))

(defn part-1 [input-range]
  (count (filter validate-password input-range)))

(comment
  (println (part-1 input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn has-2-grouped-repeating? [coll]
  (let [parts (->> coll
                   (partition-by identity)
                   (map count))]
    (and (> 6 (count parts))
         (seq (filter #{2} parts)))))

(defn validate-password-2 [nums]
  (let [nums (->> nums str (map (comp read-string str)))]
    (and (has-increasing-digits? nums)
         (has-2-grouped-repeating? nums))))

(defn part-2 [input-range]
  (count (filter validate-password-2 input-range)))

(comment
  (println (part-2 input)))

(ns day05-alchemical-reduction.core)

(def input (map int (drop-last (seq (slurp "input.txt")))))

(def reactive-pairs (zipmap (range 97 123) (range 65 91)))

(defn reactive? [[n1 n2]]
  (or (= (get reactive-pairs n1) n2)
      (= (get reactive-pairs n2) n1)))

(defn reactions [chain]
  (loop [[a b & rest] chain
         next-chain []]
    (if b
      (if (reactive? [a b])
        (recur (concat next-chain rest) [])
        (recur (cons b rest) (conj next-chain a)))
      (if a (inc (count next-chain)) (count next-chain)))))

(comment
  (time (reactions input)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn find-shortest-polymer [chain]
  (->>
   (reduce (fn [result pair]
             (assoc result
                    (keyword (str (first pair)))
                    (reactions (remove pair chain))))
           {}
           (map set reactive-pairs))
   (sort-by val)
   first))

(comment
  ;;;  very long time!
  (find-shortest-polymer input))

(ns day15-dueling-generators.core)

(def sample [65 8921])

(def divider 2147483647)
(def gen-factors {:A  16807 :B 48271})

(defn last-bits [n]
  (take-last 16 (Integer/toBinaryString n)))

(defn next-val [gen val]
  (mod
   (* val (gen-factors gen))
   divider))

(def next-vals (partial mapv next-val [:A :B]))

(defn matching? [[a b]] (= (last-bits a) (last-bits b)))

(defn find-pairs [n input]
  (loop [n n
         pair-count 0
         [v1 v2] input]
    (if (zero? n) pair-count
        (let [nxt (next-vals [v1 v2])]
          (if (matching? nxt)
            (recur (dec n) (inc pair-count) nxt)
            (recur (dec n) pair-count nxt))))))


(comment (find-pairs 40000000 sample))

(defn next-val-a [val]
  (let [nxt (fn [v] (mod (* v 16807) divider))
        start (nxt val)]
    (loop [v start]
      (if (zero? (mod v 4)) v
          (recur (nxt v))))))

(defn next-val-b [val]
  (let [nxt (fn [v] (mod (* v 48271) divider))
        start (nxt val)]
    (loop [v start]
      (if (zero? (mod v 8)) v
          (recur (nxt v))))))

(defn find-pairs-part-2 [n input]
  (loop [n n
         pair-count 0
         [v1 v2] input]
    (if (zero? n) pair-count
        (let [v11 (next-val-a v1)
              v22 (next-val-b v2)]
          (if (matching? [v11 v22])
            (recur (dec n) (inc pair-count) [v11 v22])
            (recur (dec n) pair-count [v11 v22]))))))

(comment
  (time
   (println
    (find-pairs-part-2 5000000 sample))))

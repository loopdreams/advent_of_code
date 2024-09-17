(ns day06-memory-reallocation.day06-memory-reallocation)

(def sample-blocks [0 2 7 0])

(def input-blocks [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11])

(defn reallocate [blocks]
  (let [len (count blocks)
        target (apply max blocks)
        target-idx (.indexOf blocks target)]
    (loop [b (assoc blocks target-idx 0)
           pos (mod (inc target-idx) len)
           allocate target]
      (if (zero? allocate) b
          (let [active-val (nth b pos)]
            (recur
             (assoc b pos (inc active-val))
             (mod (inc pos) len)
             (dec allocate)))))))


(defn run-reallocator [blocks]
  (loop [c 0
         configs []
         b blocks]
    (if (some #{b} configs) [c [configs b]]
        (recur (inc c)
               (conj configs b)
               (reallocate b)))))

(comment (first (run-reallocator input-blocks)))

;; part 2


(defn count-cycle [[c [configs target]]]
  (- c
     (ffirst
      (filter (fn [[_ val]]
                (= val target))
              (map-indexed vector configs)))))

(count-cycle (run-reallocator input-blocks))

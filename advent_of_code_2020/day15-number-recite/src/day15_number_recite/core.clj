(ns day15-number-recite.core)

(defn init-starting-ns [starts]
  (let [indexed (map-indexed vector starts)]
    (reduce (fn [m [idx val]]
              (assoc m val [idx]))
            {}
            indexed)))

(defn calculate-current [i said num]
  (let [[a b] (get said num)]
    (if b
      (let [time (- b a)
            [x y] (get said time)]
        [time (assoc said time (if x [(if y y x) i] [i]))])
      (let [[c d] (get said 0)]
        [0 (assoc said 0 (if c [(if d d c) i] [i]))]))))

(defn count-numbers-to-n [n start]
  (loop [i (count start)
         said (init-starting-ns start)
         num-check  (last start)]
    (if (= i n) num-check
        (let [[num new-said] (calculate-current i said num-check)]
          (recur (inc i) new-said num)))))

(comment
  (time (println (count-numbers-to-n 30000000 [15 12 0 14 3 1]))))

(ns day03-spiral-memory.core)

(defn taxi-distance [x y]
  (+ (abs x) (abs y)))

(defn target-distance [target]
  (loop [val 1 dir 0 x 0 y 0 xlim 1 ylim 1]
    (if (= val target) (taxi-distance x y)
        (case dir
          0 (if (= x xlim)
              (recur val (inc dir) x y xlim ylim)
              (recur (inc val) dir (inc x) y xlim ylim))
          1 (if (= y ylim)
              (recur val (inc dir) x y xlim ylim)
              (recur (inc val) dir x (inc y) xlim ylim))
          2 (if (= x (- xlim))
              (recur val (inc dir) x y (inc xlim) ylim)
              (recur (inc val) dir (dec x) y xlim ylim))
          3 (if (= y (- ylim))
              (recur val 0 x y xlim (inc ylim))
              (recur (inc val) dir x (dec y) xlim ylim))))))

(target-distance 368078)

;; part 2


(defn neighbours [point]
  (map (partial mapv + point) [[0 1] [0 -1] [-1 0] [1 0] [-1 -1] [1 1] [-1 1] [1 -1]]))

(defn nbr-sum [vs x y]
  (let [nbrs (set (neighbours [x y]))]
    (reduce (fn [acc [k v]]
              (if (nbrs k)
                (+ acc v)
                acc))
            0
            vs)))

(defn update-values [vals x y]
  (let [k (nbr-sum vals x y)]
    (assoc vals [x y] k)))

(defn highest-val [vs]
  (-> (vals vs) sort last))

(defn target-value [target]
  (loop [vs {[0 0] 1} dir 0 x 1 y 0 xlim 1 ylim 1]
    (if (> (highest-val vs) target) (highest-val vs)
        (case dir
          0 (if (= x xlim)
              (recur vs (inc dir) x y xlim ylim)
              (recur (update-values vs x y) dir (inc x) y xlim ylim))
          1 (if (= y ylim)
              (recur vs (inc dir) x y xlim ylim)
              (recur (update-values vs x y) dir x (inc y) xlim ylim))
          2 (if (= x (- xlim))
              (recur vs (inc dir) x y (inc xlim) ylim)
              (recur (update-values vs x y) dir (dec x) y xlim ylim))
          3 (if (= y (- ylim))
              (recur vs 0 x y xlim (inc ylim))
              (recur (update-values vs x y) dir x (dec y) xlim ylim))))))

(target-value 368078)

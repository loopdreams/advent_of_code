(ns day11-dumbo-octopus.core
  (:require [clojure.string :as str]))

;; I was struggling with this so I looked up a solution from https://github.com/wevre/advent-of-code/blob/master/src/advent_of_code/2021/day_11.clj
;; It turned out I had read the question wrong and was gettling ALL verticals/horizontals/diagonals for the update, when you only
;; needed ADJACENT. So, for now, I've mostly relied on the solution about. Maybe at some point I will go back to my
;; own approach and fix it (it was similar, only using reduce in the step function, with the queue being reduced into the map of points)
;; I also had a more complicated initial map [{:loc [x y] :val 5}], because I didn't realise you could use a vector
;; as a key in a map! So, at least this was a good learning experience. now, the map is [[x y], val], much simpler to work with

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn str->ints [s]
  (->> s
       (seq)
       (mapv #(Integer/parseInt (str %)))))

(defn parse-input [input]
  (let [lines (mapv str->ints (str/split-lines input))]
    (reduce conj {}
            (reduce concat
                    (for [i (range 0 (count lines))]
                      (for [j (range 0 (count (first lines)))
                            :let [val (nth (nth lines i) j)]]
                        [[i j] val]))))))

(defn neighbors [[r c]]
  (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)]
    [(+ r dr) (+ c dc)]))


(defn one-step
  "I was struggling with this, so I took the part about marking flashes with :f
  from https://github.com/wevre/advent-of-code/blob/master/src/advent_of_code/2021/day_11.clj.
  and merged othert parts of that approach into my own similar approach"
  ([points] (one-step points (keys points)))
  ([points to-update]
   (if-let [p (first to-update)]
     (cond
       (= :f (points p)) (recur points (next to-update))
       (<= 9 (points p)) (recur (assoc points p :f)
                                (into (next to-update)
                                      (->> (neighbors p)
                                           (select-keys points)
                                           keys)))
       :else (recur (update points p inc) (next to-update)))
     (reduce-kv (fn [m k v] (assoc m k (if (= :f v) 0 v))) {} points))))


(comment

  ;; PART 1
  (->> (parse-input input)
       (iterate one-step)
       (drop 1)
       (take 100)
       (map #(count (keep #{0} (vals %))))
       (reduce +)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn find-sync-step [input]
  (let [points (parse-input input)]
    (loop [p (one-step points)
           step 1]
      (if (= 100 (count (keep #{0} (vals p))))
        step
        (recur (one-step p) (inc step))))))

(comment (find-sync-step input))
      
   




(comment

  (defn update-points [points to-update]
    (reduce (fn [ps p]
              (let [updated-point (update p :val inc)]
                (cond
                  (> (:val updated-point) 9) (let [reset (assoc updated-point :val 0)
                                                   new-points (update-connected
                                                               (filter
                                                                #((set (connected-points (:pos p))) (:pos %))
                                                                ps)
                                                               (remove #{p} ps))]
                                               (conj new-points reset))
                  (= 0 (:val p)) ps
                  :else (conj ps updated-point))))
            points to-update))


  ;; (defn do-step [points]
  ;;   (let [first-pass (reverse (sort-by :val (update-first-pass points)))]
  ;;     (loop [[p & ps] first-pass]
  ;;       (if (> (:val p) 9)
  ;;         (let [update-with-conns (update-connected
  ;;                                  (connected-points (:pos p))
  ;;                                  ps)]
  ;;           (recur
  ;;            (reverse (sort-by :val
  ;;                              (conj update-with-conns (assoc p :val 0))))))
  ;;         (conj ps p)))))
  ;;
  (defn step-update
    ([points] (step-update points points))
    ([points to-update]
     (let [updated (reverse (sort-by :val (update-points points to-update)))]
       (if (> (:val (first updated)) 9)
         (let [to-update (filter #(> (:val %) 9) updated)]
           (recur updated to-update))
         updated))))




  (comment
    (defn do-step [points]
      (let [updated (reduce (fn [ps p] (update-point ps p)) points points)]
        (if (> (->> updated
                    (map :val)
                    sort
                    reverse
                    first) 9)
          (update-connected (filter #(> (:pos %))))
          update)))

    (do-step (parse-input sample)))



  (defn update-connected [connects points]
    (if (empty? connects) points
        (let [[{val :val :as p} & cs] connects
              p-new (if (zero? val) p (update p :val inc))]
          (recur cs
                 (conj (remove #{p} points) p-new)))))



  (comment
    (do-step (do-step (parse-input sample)))))

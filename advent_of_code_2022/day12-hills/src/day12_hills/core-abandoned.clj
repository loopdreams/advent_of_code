(ns day12-hills.core-abandoned
  (:require [clojure.string :as s]))

(def inf (dec (Long/MAX_VALUE)))

(def elevations
  (->> "input.txt"
       (slurp)
       (s/split-lines)
       (map seq)
       (map vec)
       (into [])))

(defn find-start-end [elevations start-or-end]
  (let [positions (map #(.indexOf % start-or-end) elevations)
        y (first (remove neg? positions))
        x (.indexOf positions y)]
    (keyword (str x y))))

(defn limit? [val limit]
  (or (> val (dec limit))
      (< val 0)))

(defn map-row [row x width start end]
  (loop [[y & ys] (range 0 width)
         r row
         map {}]
    (if y (recur
           ys
           (rest r)
           (assoc map (keyword (str x y))
                  (zipmap [:elevation :coords]
                          [(condp = (int (first r))
                             (int start) 96
                             (int end)   123
                             (int (first r)))
                           [x y]])))
        map)))

(defn out-of-bounds? [[x y]]
  (let [w (count (first elevations))
        h (count elevations)]
    (not (and (>= x 0)
              (>= y 0)
              (< x h)
              (< y w)))))

(defn coords->key [[x y]]
  (keyword (str x y)))

(defn lookup-el [key graph]
  (get-in graph [key :elevation]))

(defn add-neighbours
  [node graph]
  (let [{[x y] :coords elevation :elevation} (node graph)
        valid-elevation (range (dec elevation) (+ 2 elevation))
        neighbours [[(dec x) y]
                    [(inc x) y]
                    [x (dec y)]
                    [x (inc y)]]
        n-in        (remove #(out-of-bounds? %) neighbours)
        n-keys      (map coords->key n-in)
        eles  (map #(lookup-el % graph) n-keys)]
    (loop [es eles
           keys n-keys
           result []]
      (if (empty? es) (-> graph (assoc-in [node :neighbours] result))
          (if (some #{(first es)} valid-elevation)
            (recur (rest es) (rest keys) (conj result (first keys)))
            (recur (rest es) (rest keys) result))))))


(defn add-additional-keys [graph keys]
  (if (seq keys)
    (recur
     (add-neighbours (first keys) graph)
     (rest keys))
    graph))

(defn all-keys [graph]
  (sort (keys graph)))

(defn create-graph [elevations start end]
  (let [width (count (first elevations))]
    (loop [[row & rows] elevations
           graph {}
           x 0]
      (if row
        (let [row-map (map-row row x width start end)]
          (recur
           rows
           (merge graph row-map)
           (inc x)))
        (add-additional-keys graph (all-keys graph))))))

(defn find-start-node [graph]
  (for [[node {value :elevation}] graph
        :when (= value (int 96))]
    node))

(defn find-end-node [graph]
  (for [[node {value :elevation}] graph
        :when (= value (int 123))]
    node))

(defn neighbours
  [graph node unvisited]
  (for [ns (get-in graph [node :neighbours])
        :when (some #{ns} unvisited)]
    ns))


(defn set-cost [costs cost-of-current nbr]
  (let [curr-nbr (costs nbr)
        val (if (< curr-nbr (inc cost-of-current))
              curr-nbr
              (inc cost-of-current))]
    (assoc-in costs [nbr] val)))

(defn update-costs [graph costs curr unvisited]
  (loop [nbs (neighbours graph curr unvisited)
         c costs]
    (if (empty? nbs) c
        (recur (rest nbs)
               (set-cost c (costs curr) (first nbs))))))


(defn find-path
  "Dijkstra functions from loganlinn
  -https://gist.github.com/loganlinn/5437067"
  [elevations start end]
  (let [graph      (create-graph elevations start end)
        start-node (first (find-start-node graph))
        end-node   (first (find-end-node graph))]
    (println end-node)
    (loop [costs     (assoc (zipmap (keys graph) (repeat inf)) start-node 0)
           curr      start-node
           unvisited (disj (apply hash-set (keys graph)) start-node)]
      (if (or (empty? unvisited) (= inf (costs curr)))
        costs
        (let [new-costs (update-costs graph costs curr unvisited)
              new-curr  (first (sort-by new-costs unvisited))]
          (if (= end-node curr)
            (new-costs end-node)
            (recur new-costs
                   new-curr
                   (disj unvisited new-curr))))))))



(def result1 (find-path elevations \S \E))

(result1 :25153)


;; (elevations "input.txt")

;; (->> "elevations.txt"
;;      (slurp)
;;      (s/split-lines)
;;      (map seq)
;;      (map vec)
;;      (into []))

      


















;; Dijkstra functions from loganlinn -https://gist.github.com/loganlinn/5437067
(comment
  (def ^:private inf (Long/MAX_VALUE))

  (defn neighbors
    "Returns n's neighbors, optionally filtered if unvisited"
    ([g n] (get g n {}))
    ([g n uv] (select-keys (neighbors g n) uv)))



  (defn update-costs
    "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
    [g costs curr unvisited]
    (let [curr-cost (costs curr)]
      (reduce
       (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
       costs
       (neighbors g curr unvisited))))

  (defn dijkstra
    "Returns a mapping of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a mapping of nodes to map of neighboring nodes and associated cost.
  Optionally, specify :target node to return only the min price for target"
    [g src & {:keys [target]}]
    (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
           curr src
           unvisited (disj (apply hash-set (keys g)) src)]
      (if (or (empty? unvisited) (= inf (costs curr)))
        costs
        (let [costs' (update-costs g costs curr unvisited)
              curr' (first (sort-by costs' unvisited))]
          (if (= target curr)
            (costs' target)
            (recur costs'
                   curr'
                   (disj unvisited curr')))))))


  (defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!")))



;; (defn lookup-val [[x y] elevations]
;;   (int (nth (nth elevations x) y)))


;; (defn find-start-end [elevations start-or-end]
;;   (let [positions (map #(.indexOf % start-or-end) elevations)
;;         y (first (remove neg? positions))
;;         x (.indexOf positions y)]
;;     [x y]))

;; (defn limit? [val limit]
;;   (or (> val (dec limit))
;;       (< val 0)))



;; ;; TODO at the moment this isn't optimal. Instead of this 'touching' fn,
;; ;; you could have it 'try' each direction in turn,
;; ;; with a bias toward moving toward 'end'
;; (defn touching
;;   [[x y] elevations]
;;   (let [v-limit (count elevations)
;;         h-limit (count (first elevations))]
;;     (loop [points [(inc x) (inc y) (dec x) (dec y)]
;;            count 4
;;            result []]
;;       (if (zero? count) result
;;           (if (even? count)
;;             (recur
;;              (rest points)
;;              (dec count)
;;              (if (limit? (first points) v-limit)
;;                result
;;                (conj result [(first points) y])))
;;             (recur
;;              (rest points)
;;              (dec count)
;;              (if (limit? (first points) h-limit)
;;                result
;;                (conj result [x (first points)]))))))))




;; (defn sort-options [candidates values]
;;   (reverse (sort-by second (partition 2 (interleave candidates values)))))

;; (defn find-next-step [cur-pos elevations previous-positions]
;;   (let [p-val (lookup-val cur-pos elevations)
;;         candidates (touching cur-pos elevations)
;;         c-vals (map #(lookup-val % elevations) candidates)
;;         sorted (sort-options candidates c-vals)]
;;     (loop [s sorted
;;            result []]
;;       (if (= 1 (count s)) (if (empty? result) (first (first s))  (first result))
;;           (cond
;;             (= 1 (- (second (first s)) p-val))
;;             (recur
;;              (rest s)
;;              (conj result (first (first s))))
;;             (and (= 0 (- (second (first s)) p-val))
;;                  (not (some #{(first (first s))} previous-positions))
;;                  (empty? result))
;;             (recur
;;              (rest s)
;;              (conj result (first (first s))))
;;             :else (recur
;;                    (rest s)
;;                    result))))))

;; (defn find-path [elevations]
;;   (let [end (int \E)]
;;     (loop [path [(find-start elevations)]
;;            count 0]
;;       (let [cur-pos (last path)
;;             cur-val (lookup-val cur-pos elevations)]
;;         (if (= cur-val end) path
;;             (recur
;;              (conj path (find-next-step cur-pos elevations path))
;;              (inc count)))))))

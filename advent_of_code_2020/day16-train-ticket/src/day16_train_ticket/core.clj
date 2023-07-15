(ns day16-train-ticket.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.data.priority-map :refer [priority-map]]))

(def input (slurp "input.txt"))
(def sample (slurp "sample.txt"))

(defn parse-classes [classes]
  (into {} (for [line (str/split-lines classes)
                 :let [[_ type a b x y] (re-find #"(\w+ ?\w+): (\d+)-(\d+) or (\d+)-(\d+)" line)
                       [ra rb rx ry] (map parse-long [a b x y])]]
             [(keyword (str/replace type " " "-"))
              (into []
                    (concat (range ra (inc rb))
                            (range rx (inc ry))))])))

(defn parse-ticket-fields [str]
  (for [line (rest (str/split-lines str))
        :let [num-str (str/split line #",")]]
    (map parse-long num-str)))

(defn parse-input [input]
  (let [[classes your-ticket nearby-tickets] (str/split input #"\n\n")]
    {:classes (parse-classes classes)
     :your-ticket (parse-ticket-fields your-ticket)
     :nearby-tickets (parse-ticket-fields nearby-tickets)}))


(defn all-valid-nums [classes]
  (reduce (fn [collected [_ range]]
            (into collected range))
          #{}
          classes))

(defn invalid-values [{:keys [classes nearby-tickets]}]
  (let [valid (all-valid-nums classes)]
    (reduce (fn [collected nearby]
              (into collected
                    (remove valid nearby)))
            []
            nearby-tickets)))

(defn part-1 [input]
  (let [parsed (parse-input input)]
    (apply + (invalid-values parsed))))

(comment (part-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn discard-invalid [{:keys [classes nearby-tickets] :as input}]
  (let [valid (all-valid-nums classes)
        filtered (filter (fn [el] (empty? (remove valid el)))
                         nearby-tickets)]
    (assoc input :nearby-tickets filtered)))

(defn matching-positions [[_ range] positions]
  (let [pos (map-indexed vector positions)]
    (->> pos
         (filter (fn [[_ p]] (some #{p} range)))
         (map first)
         set)))

(defn find-possible-class-positions [{:keys [classes nearby-tickets]}]
  (loop [[c & cs] classes
         pos      {}]
    (if-not c pos
            (let [possible  (map #(matching-positions c %) nearby-tickets)
                  intersect (apply set/intersection possible)]
              (recur cs (assoc pos (first c) intersect))))))

(defn count-freq-positions
  "A single 'position' looks like {:class-name #{0 3 4 ..}}
  Count how many times a position occurs across all the classes and put into priotity map."
  [positions]
  (loop [[e & entries] positions
         freqs (priority-map)]
    (if-not e freqs
            (let [[k vals] e]
              (recur entries
                     (reduce (fn [m v]
                               (update m v (fnil inc 0)))
                             freqs
                             vals))))))

(defn assign-positions
  [data]
  (loop [possible (->> data find-possible-class-positions)
         queue    (->> possible count-freq-positions)
         actual   {}]
    (if (empty? queue) actual
        (let [[pos _]   (peek queue)
              [class _] (first
                         (filter (fn [el]
                                   (some #{pos} (val el)))
                                 possible))]
          (recur
           (dissoc possible class)
           (pop queue)
           (assoc actual class pos))))))
                                    
(defn part-2 [input]
  (let [data             (discard-invalid (parse-input input))
        targets          (filter #(re-find #"departure" (str %)) (keys (:classes data)))
        positions        (filter (fn [el] (some #{(key el)} targets))
                                 (assign-positions data))
        your-ticket-vals (first (:your-ticket data))]
    (reduce * (for [p    positions
                    :let [idx (val p)]]
                (nth your-ticket-vals idx)))))

(comment
  (println (part-2 input)))



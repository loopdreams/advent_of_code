(ns day08-registers.core
  (:require
   [clojure.string :as str]))

(def input (slurp "D08.txt"))

(defn parse-line [line]
  (let [[k dir amt _ t1 cond tval] (str/split line #" ")]
    {:tgt k
     :tr [(keyword dir) (parse-long amt)]
     :test [t1 cond (parse-long tval)]}))


(defn parse-input [input]
  (let [inputs (map parse-line (str/split-lines input))
        ks (map :tgt inputs)
        k-vals (reduce #(assoc %1 %2 0) {} ks)]
    [k-vals inputs]))



(defn run-instructions [input]
  (let [[k-vals inputs] (parse-input input)]
    (loop [k-vals   k-vals
           [x & xs] inputs
           max 0]
      (if-not x [(-> k-vals vals sort last) max]
        (let [{:keys [tgt tr test]} x
              [k type amt]          test
              test-fn               (case type
                                      "!=" not=
                                      ">=" >=
                                      "<=" <=
                                      "==" =
                                      "<"  <
                                      ">"  >
                                      (println type))]
          (if (test-fn (k-vals k) amt)
            (let [[type amt] tr
                  new-vals (case type
                             :dec (update k-vals tgt - amt)
                             :inc (update k-vals tgt + amt))
                  new-max (-> new-vals vals sort last)]
              (recur
               new-vals
               xs
               (if (> new-max max) new-max max)))
            (recur k-vals xs max)))))))

(defn part-1-2 [input]
  (let [[p1 p2] (run-instructions input)]
    (println "Part 1: " p1)
    (println "Part 2: " p2)))

(part-1-2 input)

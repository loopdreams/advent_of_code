(ns day02-dive.core
  (:require [clojure.string :as str]))


(defn parse-input [input]
  (let [lines (->> input
                   (str/split-lines))]
    (for [l lines
          :let [[direction number] (str/split l #" ")
                entry (hash-map :direction direction
                                :number (Integer/parseInt number))]]
      entry)))

(defn move-submarine [[init-depth init-forward] input]
  (loop [instructions (parse-input input)
         location {:pos init-forward :depth init-depth}]
    (if (seq instructions)
      (let [{d :direction n :number} (first instructions)
            {p :pos dept :depth} location]
        (case d
          "forward" (recur (rest instructions)
                           (assoc-in location [:pos] (+ p n)))
          "down"    (recur (rest instructions)
                           (assoc-in location [:depth] (+ dept n)))
          "up"      (recur (rest instructions)
                           (assoc-in location [:depth] (- dept n)))))
      (let [depth (:depth location)
            pos   (:pos location)]
        (* depth pos)))))

(comment
  (move-submarine [0 0] (slurp "input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
          

(defn move-sub-with-aim [input]
  (loop [[{d :direction n :number} & rest] (parse-input input)
         location {:pos 0 :depth 0 :aim 0}]
    (if d
      (let [{p :pos depth :depth a :aim} location]
        (case d
          "forward" (recur rest
                           (-> location
                               (assoc-in [:pos] (+ p n))
                               (assoc-in [:depth] (+ depth (* n a)))))
          "down"    (recur rest
                           (assoc-in location [:aim] (+ a n)))
          "up"      (recur rest
                           (assoc-in location [:aim] (- a n)))))
      (let [depth (:depth location)
            pos (:pos location)]
        (* depth pos)))))

(comment
  (println (move-sub-with-aim (slurp "input.txt"))))

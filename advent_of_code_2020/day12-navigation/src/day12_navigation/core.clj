(ns day12-navigation.core
  (:require [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (for [l lines
          :let [[_ com val](re-find #"(\w)(\d+)" l)]]
      [(keyword com) (Integer/parseInt val)])))

(defn turn [degrees facing direction]
  (let [facing-vals {:north 0 :east 90 :south 180 :west 270}
        v (facing facing-vals)
        new-v (mod (direction v degrees) 360)]
    (first (keep
            #(when (= (val %) new-v) (key %))
            facing-vals))))

(defn move-ship [{:keys [facing] :as current-pos} [com val]]
  (case com
    :N (update current-pos :vertical + val)
    :S (update current-pos :vertical - val)
    :E (update current-pos :horizontal + val)
    :W (update current-pos :horizontal - val)
    :L (assoc current-pos :facing (turn val facing -))
    :R (assoc current-pos :facing (turn val facing +))
    :F (move-ship current-pos
                  (case facing
                    :north [:N val]
                    :south [:S val]
                    :east  [:E val]
                    :west  [:W val]))))

(defn distance-from-origin [input]
  (let [commands (parse-input input)
        pos (reduce move-ship
                    {:facing :east
                     :horizontal 0
                     :vertical 0}
                    commands)
        x-val (:horizontal pos)
        y-val (:vertical pos)]
    (+ (abs x-val) (abs y-val))))

(comment (distance-from-origin input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn rotate [x y amount]
  (case amount
    90 [(- y) x]
    180 [(- x) (- y)]
    270 [y (- x)]))

(defn update-map [{{:keys [x y]} :waypoint :as current-pos} [com val]]
  (case com
    :N (update-in current-pos [:waypoint :y] + val)
    :S (update-in current-pos [:waypoint :y] - val)
    :E (update-in current-pos [:waypoint :x] + val)
    :W (update-in current-pos [:waypoint :x] - val)
    :L (let [[new-x new-y] (rotate x y val)]
         (-> current-pos
             (assoc-in [:waypoint :x] new-x)
             (assoc-in [:waypoint :y] new-y)))
    :R (update-map current-pos [:L (- 360 val)])
    :F (-> current-pos
           (update-in [:ship :x] + (* x val))
           (update-in [:ship :y] + (* y val)))))

(defn distance-from-origin-2 [input]
  (let [commands (parse-input input)
        pos (reduce update-map
                    {:waypoint {:x 10 :y 1}
                     :ship {:x 0 :y 0}}
                    commands)
        x-val (:x (:ship pos))
        y-val (:y (:ship pos))]
    (+ (abs x-val) (abs y-val))))

(comment (distance-from-origin-2 input))

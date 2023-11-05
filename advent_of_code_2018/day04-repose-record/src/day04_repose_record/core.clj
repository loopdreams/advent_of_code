(ns day04-repose-record.core
  (:require
    [clojure.string :as str]))

(def input (slurp "input.txt"))

(defn parse-input [input] (sort (str/split-lines input)))

(defn datestamp [s] (apply str (rest (re-find #"-(\d+)-(\d+)" s))))
(defn hr-min [s] (rest (re-find #"(\d+):(\d+)" s)))

(defn minute [s] (last (hr-min s)))
(defn hour [s] (first (hr-min s)))

(defn guard [s] (last (re-find #"#(\d+)" s)))

(defn type? [s]
  (let [t (apply str (take 2 (reverse (str/split s #" "))))]
    (case t
      "shiftbegins" :begin
      "asleepfalls" :sleep
      "upwakes"     :wakes)))

(defn build-timeline [lines]
  (loop [[l & ls]     lines
         timeline     {}
         g            nil
         asleep-start nil]
    (if l
      (case (type? l)

        :begin (recur ls timeline (guard l) asleep-start)

        :sleep (recur ls timeline g (if (= (hour l) "00")
                                      (parse-long (minute l))
                                      asleep-start))

        :wakes (recur ls (update-in timeline
                                    [g (datestamp l)]
                                    (fnil into [])
                                    (range asleep-start (parse-long (minute l))))
                      g
                      asleep-start))
      timeline)))

(defn sum-minutes [entry]
  (->> entry
       val
       (map val)
       (map count)
       (apply +)))

(defn find-highest-total-mins [timelines]
  (reduce (fn [result entry]
            (let [total-mins (sum-minutes entry)
                  cur-highest (:minutes result)]
              (if (> total-mins cur-highest)
                (-> result
                    (assoc :id (key entry)
                           :minutes total-mins))
                result)))
          {:id nil :minutes 0}
          timelines))

(defn find-most-frequent-minute [mins]
  (->> mins
       (map val)
       (reduce concat)
       sort
       (partition-by identity)
       (sort-by count)
       reverse
       first))

(defn strategy-1 [input]
  (let [timelines    (->> input
                          parse-input
                          build-timeline)
        most-asleep  (:id (find-highest-total-mins timelines))
        frequent-min (first (find-most-frequent-minute (get timelines most-asleep)))]
    (* (parse-long most-asleep) frequent-min)))

(comment
  (strategy-1 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn strategy-2 [input]
  (let [timelines (-> input parse-input build-timeline)

        [id min]  (->>
                   (reduce (fn [result entry]
                             (assoc result
                                    (key entry)
                                    (find-most-frequent-minute (val entry))))
                           {}
                           timelines)
                   (sort-by (comp count val))
                   last)]

    (* (parse-long id) (first min))))

(comment
  (strategy-2 input))

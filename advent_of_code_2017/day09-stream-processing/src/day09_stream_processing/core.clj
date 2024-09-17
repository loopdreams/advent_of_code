(ns day09-stream-processing.core)

(def input (slurp "D09.txt"))

(defn group-count [stack]
  (loop [[a & rest] stack
         temp []
         cnt 0]
    (if-not a cnt
            (if (and (= (last temp) \{)
                     (= a \}))
              (recur rest (pop temp) (+ cnt (count temp)))
              (recur rest (conj temp a) cnt)))))

(defn parse-string [str]
  (let [chars (seq str)]
    (loop [[c & cs] chars
           stack []
           garbage-true 0
           ignore-true 0]
      (if-not c
        (group-count stack)
        (cond
          (= 1 ignore-true) (recur cs stack garbage-true 0)

          (= 1 garbage-true)
          (case c
            \> (recur cs stack 0 ignore-true)
            \! (recur cs stack garbage-true 1)
            (recur cs stack garbage-true ignore-true))

          :else
          (case c
            \{ (recur cs (conj stack c) garbage-true ignore-true)
            \} (recur cs (conj stack c) garbage-true ignore-true)
            \< (recur cs stack 1 ignore-true)
            \! (recur cs stack garbage-true 1)
            (recur cs stack garbage-true ignore-true)))))))

(comment (parse-string input))

;; Part 2

(defn count-garbage [str]
  (let [chars (seq str)]
    (loop [[c & cs] chars
           garbage 0
           garbage-true 0
           ignore-true 0]
      (if-not c
        garbage
        (cond
          (= 1 ignore-true) (recur cs garbage garbage-true 0)
          (= 1 garbage-true)
          (case c
            \> (recur cs garbage 0 ignore-true)
            \! (recur cs garbage garbage-true 1)
            (recur cs (inc garbage) garbage-true ignore-true))
          :else
          (case c
            \< (recur cs garbage 1 ignore-true)
            \! (recur cs garbage garbage-true 1)
            (recur cs garbage garbage-true ignore-true)))))))

(comment
  (count-garbage input))

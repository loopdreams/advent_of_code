(ns day18-operation-order.core
  (:require [clojure.string :as str]))

;; Messy solutions ... clean up another time

(def input (slurp "input.txt"))

(def cparen \))
(def oparen \()

(declare do-maths-on-line)

(defn handle-parens [stack]
  (let [groups (partition-by #(= % \() stack)
        op (last groups)]
    [(drop-last (flatten (drop-last groups)))
     (do-maths-on-line op)]))

(defn char->int [char]
  (if (int? char) char
      (parse-long (str char))))

(defn handle-stack
  ([a b c]
   (let [operator (resolve (symbol (str b)))
         a (char->int a)
         c (char->int c)]
     [(operator a c)]))
  ([a b c & rest]
   (let [add (char->int (last rest))
         multiply (map char->int
                       (take-nth 2 (concat [a b c] (drop-last rest))))]
     [(apply * (concat [(+ add (last multiply))] (drop-last multiply)))])))

(defn do-maths-on-line [line]
  (loop [[c & chars] line
         paren-stack []
         stack       []]
    (if-not c
      (first stack)
      (if (= \( (first paren-stack))
        (cond
          (= c \)) (let [[parens sum] (handle-parens paren-stack)]
                     (if (seq parens)
                       (recur chars (into [] (concat parens [sum])) stack)
                       (recur chars [] (if (empty? stack)
                                         (conj stack sum)
                                         (apply handle-stack (conj stack sum))))))
          :else    (recur chars (conj paren-stack c) stack))

        (cond
          (= c \() (recur chars (conj paren-stack c) stack)
          (and (int? (parse-long (str c)))
               (seq stack))
          (recur chars paren-stack (apply handle-stack (conj stack c)))
          :else    (recur chars paren-stack (conj stack c)))))))


(defn part-1 [input]
  (let [lines (map #(str/replace % #" " "") (str/split-lines input))]
    (apply + (map do-maths-on-line lines))))

(comment
  (println "hello")
  (println (part-1 input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2


(declare maths-multiply-last)

(defn handle-parens-part-2 [stack]
  (let [groups (partition-by #(= % \() stack)
        op (last groups)]
    [(drop-last (flatten (drop-last groups)))
     (maths-multiply-last op)]))

(defn sum-stack
  "take last int from stack - 2nd last position -, add to c, and add back into stack."
  [stack c]
  (let [[x _] (drop (- (count stack) 2) stack)
        head  (into [] (drop-last 2 stack))]
    (conj head (apply + (map char->int [c x])))))

(defn stack-multiply
  "Last step, only values left should be multiplied together"
  [stack]
  (apply * (map char->int (take-nth 2 stack))))

(defn maths-multiply-last [line]
  (loop [[c & chars] line
         paren-stack []
         stack []]
    (if c
      (if (= oparen (first paren-stack))
        (cond

          (= c cparen)
          (let [[parens sum] (handle-parens-part-2 paren-stack)]
            (if (seq parens)
              (recur chars (into [] (concat parens [sum])) stack)
              (recur chars [] (if (= (last stack) \+) (sum-stack stack sum)
                                  (conj stack sum)))))

          :else (recur chars (conj paren-stack c) stack))

        (cond
          (= c oparen) (recur chars (conj paren-stack c) stack)

          (int? (char->int c))
          (if (= (last stack) \+)
            (recur chars paren-stack (sum-stack stack c))
            (recur chars paren-stack (conj stack c)))

          :else (recur chars paren-stack (conj stack c))))
      (stack-multiply stack))))
            

(defn part-2 [input]
  (let [lines (map #(str/replace % #" " "") (str/split-lines input))]
    (apply + (map maths-multiply-last lines))))

(comment
  (println (part-2 input)))

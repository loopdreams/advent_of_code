(ns day10-syntax-scoring.core
  (:require
   [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))
(def openings #{\{ \[ \( \<})
(def closings #{\} \] \) \>})

(defn parse-input [input]
  (str/split-lines input))

(defn matches? [closing opening]
  (case closing
    \] (= opening \[)
    \} (= opening \{)
    \) (= opening \()
    \> (= opening \<)))

(defn m-error [actual expected]
    (println "Error. Expected " expected ", received " actual)
    actual)

(defn check-line [str]
  (loop [[char & chars] (seq str)
         stack []]
    (if char
      (cond
        (and (empty? stack) (closings char)) (m-error char "opening")
        (openings char) (recur chars (conj stack char))
        (and (closings char) (matches? char (peek stack)))
        (recur chars (pop stack))
        :else (m-error char (pop stack)))
      :OK)))
(defn error-scores [char]
  (case char
    \] 57
    \} 1197
    \) 3
    \> 25137
    :OK 0))

(defn syntax-error-score [input]
  (let [lines (parse-input input)]
    (apply + (map #(error-scores (check-line %)) lines))))

(comment
  (syntax-error-score input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn check-line-part2 [str]
  (loop [[char & chars] (seq str)
         stack []]
    (if char
      (cond
        (and (empty? stack) (closings char)) nil
        (openings char) (recur chars (conj stack char))
        (and (closings char) (matches? char (peek stack)))
        (recur chars (pop stack))
        :else nil)
      (reverse stack))))

(defn completion-scores [char]
  (case char
    \( 1
    \[ 2
    \{ 3
    \< 4))

(defn part-2-scores [chars]
  (loop [[char & rest] chars
         total 0]
    (if char
      (let [new-total (+ (* total 5) (completion-scores char))]
        (recur rest new-total))
      total)))

(defn part-two-completions [input]
  (let [lines (parse-input input)
        valid (remove nil? (map check-line-part2 lines))
        mid (int (Math/floor (/ (count valid) 2)))]
    (nth (sort (map part-2-scores valid)) mid)))

(comment (part-two-completions input))

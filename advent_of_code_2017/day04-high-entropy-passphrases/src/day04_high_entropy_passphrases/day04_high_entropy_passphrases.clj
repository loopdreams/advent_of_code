(ns day04-high-entropy-passphrases.day04-high-entropy-passphrases
  (:require
   [clojure.string :as str]))

(defn valid-passphrase? [passphrase]
  (let [words (re-seq #"\w+" passphrase)]
    (->> words
         frequencies
         vals
         (apply =))))

(->> (slurp "D04.txt")
     (str/split-lines)
     (filter valid-passphrase?)
     count)

;; part 2

(defn is-anagram? [w1 w2]
  (= (sort w1) (sort w2)))

(defn anagram-free? [passphrase]
  (let [words (re-seq #"\w+" passphrase)]
    (loop [[w & ws] words]
      (if-not w true
              (let [cands (filter (fn [el] (is-anagram? w el))
                                  (remove #{w} words))]
                (when (empty? cands)
                  (recur ws)))))))

(->> (slurp "D04.txt")
     (str/split-lines)
     (filter valid-passphrase?)
     (filter anagram-free?)
     count)


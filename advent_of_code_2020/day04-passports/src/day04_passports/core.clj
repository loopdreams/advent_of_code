(ns day04-passports.core
  (:require
    [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def required-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid :cid})
(def loosely-required-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn parse-passport [passport]
  (let [fields (map #(str/split % #":")
                    (-> passport
                        (str/split #" ")))]
    (into {} (for [f fields
                   :let [key (keyword (first f))
                         val (second f)]]
               [key val]))))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (partition-by #(= % ""))
       (remove #{'("")})
       (map #(str/join #" " %))
       (map parse-passport)))


(defn check-for-valid-fields [input]
  (let [passports (parse-input input)]
    (count (for [p passports
                 :when (or (= (set (keys p)) required-fields)
                           (= (set (keys p)) loosely-required-fields))]
             p))))

(comment
  (check-for-valid-fields input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn valid-fields? [p]
  (or (= (set (keys p)) required-fields)
      (= (set (keys p)) loosely-required-fields)))

(defn valid-byr? [{year :byr}]
  (some #{(Integer/parseInt year)} (range 1920 2003)))

(defn valid-iyr? [{issued :iyr}]
  (some #{(Integer/parseInt issued)} (range 2010 2021)))

(defn valid-eyr? [{exp :eyr}]
  (some #{(Integer/parseInt exp)} (range 2020 2031)))

(defn valid-hgt? [{height :hgt}]
  (let [[_ val unit] (re-find #"(\d+)(\w+)" height)]
    (case unit
      "cm" (some #{(Integer/parseInt val)} (range 150 194))
      "in" (some #{(Integer/parseInt val)} (range 59 76))
      nil)))

(defn valid-hcl? [{colour :hcl}] (re-find #"#([A-Fa-f0-9]{6})" colour))

(defn valid-ecl? [{colour :ecl}]
  (some #{colour} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(defn valid-passport? [p]
  (and
   (valid-fields? p)
   (valid-byr? p)
   (valid-iyr? p)
   (valid-eyr? p)
   (valid-hgt? p)
   (valid-hcl? p)
   (valid-ecl? p)
   (= (count (:pid p)) 9)))

(defn count-valid-passports [input]
  (let [passports (parse-input input)]
    (count (filter valid-passport? passports))))

(comment
  (count-valid-passports input))

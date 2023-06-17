(ns day08-seven-segment.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def sample (slurp "sample.txt"))
(def input (slurp "input.txt"))

(defn map-line [line]
  (let [[signals digits] (str/split line #"\|")
        ds (str/split (str/trim digits) #" ")
        sigs (str/split (str/trim signals) #" ")]
    (hash-map :signals sigs :digits ds)))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (map map-line lines)))

(defn find-unique-segs [line]
  (let [{digits :digits} line]
    (apply + (let [unique-lens #{2 4 3 7}]
               (for [d digits
                     :let [len (count d)]
                     :when (unique-lens len)]
                 1)))))

(defn all-uniques [input]
  (let [data (parse-input input)]
    (apply +  (map find-unique-segs data))))

(comment

  (find-unique-segs (second (parse-input sample)))
  (all-uniques input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2

(defn gen-digit-key [digit-pos]
  (let [fn (fn [keys] (set (vals (select-keys digit-pos keys))))]
    {:zero  (fn [:top :t-left :t-right :b-left :b-right :bottom])
     :one   (fn [:t-right :b-right])
     :two   (fn [:top :t-right :mid :b-left :bottom])
     :three (fn [:top :t-right :mid :b-right :bottom])
     :four  (fn [:t-left :t-right :mid :b-right])
     :five  (fn [:top :t-left :mid :b-right :bottom])
     :six   (fn [:top :t-left :mid :b-left :b-right :bottom])
     :seven (fn [:top :t-right :b-right])
     :eight (fn [:top :t-left :t-right :mid :b-left :b-right :bottom])
     :nine  (fn [:top :t-left :t-right :mid :b-right :bottom])}))

(defn get-digit [str key]
  (let [d key
        code (set str)]
    (condp = code
      (:zero  d) 0
      (:one   d) 1
      (:two   d) 2
      (:three d) 3
      (:four  d) 4
      (:five  d) 5
      (:six   d) 6
      (:seven d) 7
      (:eight d) 8
      (:nine  d) 9)))

(defn val-top [seven one]
  (set/difference seven one))

(defn vals-mid-bottom-bleft [[two three five] eight four top]
  (let [t-mid-b      (set/intersection two three five)
        b-and-b-left (set/difference eight (set (concat four top)))
        mid-b        (set/difference t-mid-b top)
        bottom       (set/intersection mid-b b-and-b-left)
        mid          (set/difference mid-b b-and-b-left)
        b-left       (set/difference b-and-b-left mid-b)]
    [mid bottom b-left]))

(defn vals-b-right-t-right [[zero six nine] one top bottom]
  (let [t-left-b-right (->> (set/intersection zero six nine)
                            (remove top)
                            (remove bottom)
                            (set))
        b-right        (set/intersection t-left-b-right one)
        t-right        (set/difference one b-right)]
    [b-right t-right]))

(defn set-key [signals]
  (let [singles (fn [n] (->> signals
                             (filter #(= n (count %)))
                             (map seq)
                             (first)
                             (into #{})))
        groups  (fn [n] (->> signals
                             (filter #(= n (count %)))
                             (map #(into #{} (seq %)))
                             vec))
        
        one            (singles 2)
        four           (singles 4)
        seven          (singles 3)
        eight          (singles 7)
        zero-nine-six  (groups 6)
        two-three-five (groups 5)
        
        top                 (val-top seven one)
        [mid bottom b-left] (vals-mid-bottom-bleft
                             two-three-five eight four top)
        [b-right t-right]   (vals-b-right-t-right zero-nine-six one top bottom)
        t-left              (set/difference eight (set/union top mid bottom b-left b-right t-right))]
    (-> {}
        (assoc :top (first top)
               :t-left (first t-left)
               :t-right (first t-right)
               :mid (first mid)
               :b-left (first b-left)
               :b-right (first b-right)
               :bottom (first bottom)))))

(defn list->int [list]
  (->> list
       (map str)
       (str/join)
       (Integer/parseInt)))

(defn decoder [input]
  (let [data (parse-input input)]
    (apply + (map list->int
                  (for [d data
                        :let [{sigs :signals digts :digits} d
                              key (->> sigs
                                       set-key
                                       gen-digit-key)]]
                    (map #(get-digit % key) digts))))))

(comment
  (decoder input))


          


(ns user
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

;; These are just some small functions for rebuilding the Markdown index pages for each AoC year

(def aoc-dirs  (->> "." fs/list-dir (filter #(re-find #"advent" (str %))) (map str)))

(defn sub-dir-filenames [aoc-dir]
  (->> aoc-dir
       fs/list-dir
       (filter #(re-find #"day" (str %)))
       (map fs/components)
       (map last)
       (map str)))

(defn format-md-link [filename]
  (let [[_ num _ name] (re-find #"day(\d\d)(-)([\w-\s]+)" filename)]
    (str "- [Day "
         num
         " - "
         (str/join " " (map str/capitalize (str/split name #"[-_]")))
         "]"
         "("
         filename
         "/src/"
         (str/replace filename #"-" "_")
         "/core.clj)\n")))

(defn md-title [dirname]
  (let [title (map str/capitalize
                   (-> dirname
                       str
                       (str/replace #"./" "")
                       (str/split #"[_-]")))]
    (str "# " (str/join #" " title))))
      

(defn format-index [dirname]
  (let [title (md-title dirname)
        links (sort (map format-md-link
                         (sub-dir-filenames dirname)))]
    (str title "\n\n" (str/join links))))

(defn refresh-indexes []
  (map (fn [dir] (spit (str dir "/README.md")
                       (format-index dir)))
       aoc-dirs))

(comment
  (refresh-indexes))

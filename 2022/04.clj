(ns camp-cleanup
  (:require [clojure.string :as str]))

(defn inside? [[from1 to1 from2 to2]]
  (or (<= from1 from2 to2 to1)
      (<= from2 from1 to1 to2)))

(defn overlaps? [[from1 to1 from2 to2]]
  (and (>= to1 from2)
       (>= to2 from1)))

(defn process [pred]
  (->> (slurp "04.in")
       (str/split-lines)
       (map (comp pred
                  #(map parse-long %)
                  (partial re-seq #"[0-9]+")))
       (filter identity)
       (count)))

(map process [inside? overlaps?])

(ns camel-cards
  (:require [clojure.string :as str]))

(def hand-strength (zipmap [[1 1 1 1 1] [2 1 1 1] [2 2 1] [3 1 1] [3 2] [4 1] [5]] (range)))
(def card-strength (zipmap "23456789TJQKA" (range)))

(defn score [hand]
  [(hand-strength (sort-by - (vals (frequencies hand))) )
   (mapv card-strength hand)])

(defn parse-line [score-fn line]
  (let [[hand bid] (str/split line #" ")]
    {:hand hand
     :bid (parse-long bid)
     :score (score-fn hand)}))

;; part 2

(def card-strength' (zipmap "J23456789TQKA" (range)))

(defn joker-freqs [hand]
  (let [joker-count (count (filter (partial = \J) hand))]
    (if (= 5 joker-count)
      [5]
      (update (into [] (sort-by - (vals (frequencies (remove (partial = \J) hand)))))
              0 + joker-count))))

(defn score' [hand]
  [(-> hand joker-freqs hand-strength)
   (mapv card-strength' hand)])

(map #(->> (slurp "2023/07.in")
           (str/split-lines)
           (map (partial parse-line %))
           (sort-by :score)
           (map (fn [rank {:keys [bid]}] (* rank bid)) (iterate inc 1))
           (reduce +))
     [score score'])

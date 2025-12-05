(require '[clojure.string :as str])

(let [[ranges [_ & ids]]
      (->> (slurp "2025/05.in")
           (str/split-lines)
           (split-with seq))

      ranges
      (sort (mapv #(mapv parse-long (re-seq #"\d+" %)) ranges))

      ids
      (map parse-long ids)

      part-1-fresh-ids
      (count (filter (fn [id] (some (fn [[from to]] (<= from id to))
                                    ranges)) ids))

      summed-fresh-ranges
      (reduce (fn [coll [from to]]
                (or (when-let [[last-from last-to] (peek coll)]
                      (when (<= last-from from last-to)
                        (-> coll (pop) (conj [last-from (max last-to to)]))))
                    (conj coll [from to])))
              []
              ranges)

      fresh-ids-count
      (transduce (map (fn [[from to]] (inc (- to from))))
                 + 0 summed-fresh-ranges)]
  [part-1-fresh-ids fresh-ids-count])

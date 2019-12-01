(ns advent.checksum)

(defn two-and-three [str]
  (let [counts (->> str sort (partition-by identity) (map count))]
    (map #(some (partial = %) counts) [2 3])))

(let [lines (clojure.string/split-lines (slurp "02.in"))]
  (let [twos-and-threes (map two-and-three lines)]
    (println (* (count (filter identity (map first twos-and-threes)))
                (count (filter identity (map second twos-and-threes)))))))

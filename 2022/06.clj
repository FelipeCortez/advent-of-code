(ns tuning-trouble)

(def sample (slurp "06.in"))

(defn find-different [n]
  (->> sample
       (partition n 1)
       (map (comp count set))
       (map vector (iterate inc n))
       (filter (comp (partial = n) second))
       ffirst))

(map find-different [4 14])

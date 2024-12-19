(require '[clojure.string :as str])

(let [[towels _ & designs] (str/split-lines (slurp "2024/19.in"))]
  (def towels (str/split towels #", "))
  (def designs designs))

(count (keep (partial re-matches
                      (re-pattern (format "(%s)+" (str/join "|" towels))))
             designs))

(def count-possible
  (memoize
   (fn [design]
     (if (str/blank? design)
       1
       (reduce + (->> towels
                      (keep (fn [towel]
                              (when (str/starts-with? design towel)
                                (subs design (count towel)))))
                      (map count-possible)))))))

(reduce + (pmap count-possible designs))

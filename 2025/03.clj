(require '[clojure.string :as str])
(import '[java.util Collections])

(defn char->long [c] (- (int c) (int \0)))
(defn idx+first-max [coll]
  (Collections/max (map-indexed (fn [idx x] [idx x]) coll)
                   (fn [[idx1 x1] [idx2 x2]] (compare [x1 (- idx1)] [x2 (- idx2)]))))

(defn max-joltage' [coll x]
  (when-not (zero? x)
    (let [[idx first-max] (idx+first-max (subvec coll 0 (- (count coll) (dec x))))]
      (lazy-seq (cons first-max
                      (max-joltage' (subvec coll (inc idx))
                                    (dec x)))))))

(defn max-joltage [x coll]
  (->> (max-joltage' (mapv char->long coll) x)
       (apply str)
       (parse-long)))

(defn solve [f banks] (reduce + (map f banks)))

(->> (slurp "2025/03.in")
     (str/split-lines)
     ((juxt #(solve (partial max-joltage 2) %)
            #(solve (partial max-joltage 12) %))))

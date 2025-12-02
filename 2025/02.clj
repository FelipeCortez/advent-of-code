(def ranges
  (->> (slurp "2025/02.in")
       (re-seq #"\d+")
       (map parse-long)
       (partition 2)))

(defn rangei [start end] (range start (inc end)))

(defn two-equal-parts? [x]
  (let [s (str x), length (count s), half-length (/ length 2)]
    (and (even? length)
         (= (subs s 0 half-length)
            (subs s half-length)))))

;; part 1
(->> ranges
     (mapcat (fn [[start end]] (rangei start end)))
     (filter two-equal-parts?)
     (reduce + 0))

;; part 2
(def factors
  (memoize
   (fn [x]
     (for [divisor (rangei 1 (/ x 2)), :when (zero? (mod x divisor))]
       divisor))))

(defn silly-pattern? [x]
  (let [s (str x)]
    (and (>= x 10)
         (some (fn [factor] (= 1 (count (set (partition factor s)))))
               (factors (count s))))))

(->> ranges
     (mapcat (fn [[start end]] (rangei start end)))
     (filter silly-pattern?)
     (reduce + 0))

(require '[clojure.string :as str])

(defn valid-report? [xs]
  (and (or (apply < xs) (apply > xs))
       (every? (fn [[x y]] (<= 1 (abs (- x y)) 3))
               (partition 2 1 xs))))

(defn dampen-problems
  ([xs]
   (cons xs (dampen-problems [] xs)))
  ([before [head & tail]]
   (if (seq tail)
     (lazy-seq (cons (into before tail)
                     (dampen-problems (conj before head) tail)))
     [before])))

(let [reports
      (->> (slurp "2024/02.in")
           (str/split-lines)
           (map (fn [line] (map parse-long (re-seq #"\d+" line)))))]
  [(count (filter valid-report? reports))
   (count (filter #(some valid-report? (dampen-problems %)) reports))])

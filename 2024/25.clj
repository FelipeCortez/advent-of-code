(require '[clojure.string :as str])

(defn schematic-type [[[fst & _] & _]] (if (= \# fst) :keys :locks))
(defn count-fills [coll] (dec (count (filter #(= \# %) coll))))
(defn count-schematic [coll] (map count-fills coll))
(defn transpose [m] (apply mapv vector m))

(def keys+locks
  (->> "2024/25.in"
       (slurp)
       (str/split-lines)
       (partition 7 8)
       (map transpose)
       (reduce (fn [m schematic]
                 (update m (schematic-type schematic) conj (count-schematic schematic)))
               {:keys [] :locks []})))

(count
 (for [key (:keys keys+locks), lock (:locks keys+locks)
       :when (every? #(<= % 5) (mapv + key lock))]
   [key lock]))

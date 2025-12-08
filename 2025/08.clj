(require '[clojure.string :as str]
         '[clojure.math :as math]
         '[clojure.set :as set])

(def boxes
  (->> "2025/08.in"
       (slurp)
       (str/split-lines)
       (mapv (fn [line] (mapv parse-long (re-seq #"\d+" line))))))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (math/sqrt (+ (math/pow (- x1 x2) 2)
                (math/pow (- y1 y2) 2)
                (math/pow (- z1 z2) 2))))

(def boxes->circuits
  (into {}
        (map (fn [box] [box (atom #{box})]))
        boxes))

(def closest-boxes
  (sort (for [i (range         (count boxes))
              j (range (inc i) (count boxes))
              :let [a (get boxes i), b (get boxes j)]]
          [(distance a b) a b])))

;; part 1
(defn merge-circuits [boxes->circuits [_distance a b]]
  (let [circuit-a (boxes->circuits a)
        circuit-b (boxes->circuits b)
        [*smallest *largest] (sort-by (comp count deref) [circuit-a circuit-b])]
    (swap! *largest set/union @*smallest)
    (reduce (fn [boxes->circuits box] (assoc boxes->circuits box *largest))
            boxes->circuits
            @*smallest)))

(def merged-circuits
  (reduce merge-circuits boxes->circuits (take 1000 closest-boxes)))

(def part-1
  (->> merged-circuits
       (vals)
       (distinct)
       (sort-by (comp count deref) #(compare %2 %1))
       (take 3)
       (map (comp count deref))
       (reduce *)))

(def part-2
  (reduce (fn [boxes->circuits [_distance a b :as pair]]
            (let [merged-circuits (merge-circuits boxes->circuits pair)]
              (if (= (count merged-circuits)
                     (count @(first (vals merged-circuits))))
                (reduced (* (first a) (first b)))
                merged-circuits)))
          boxes->circuits
          closest-boxes))

[part-1 part-2]

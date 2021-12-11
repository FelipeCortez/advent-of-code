(def example "2199943210
3987894921
9856789892
8767896789
9899965678")

@(def coords
   (->> (slurp "09.in")
        (clojure.string/split-lines)
        (mapv (comp vec #(map (comp read-string str) %)))))

(defn v+ [v1 v2] (mapv + v1 v2))

(defn at [coords [x y]] (-> coords (get y) (get x)))

(let [max-x (count (first coords))
      max-y (count coords)
      deltas [[0 1] [0 -1] [1 0] [-1 0]]
      valid? (fn [[x y]]
               (and (<= 0 x (dec max-x))
                    (<= 0 y (dec max-y))))]
  (reduce +
          (for [y (range max-y)
                x (range max-x)
                :let [v (at coords [x y])]
                :when (->> (map v+ deltas (repeat [x y]))
                           (filterv valid?)
                           (mapv (partial at coords))
                           (every? (partial < v)))]
            (inc v))))

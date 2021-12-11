(ns smoke-basin)

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

(def deltas [[0 1] [0 -1] [1 0] [-1 0]])

(defn fill! [coords valid? visited* point]
  (vswap! visited* conj point)
  (run! (partial fill! coords valid? visited*)
        (filter (every-pred valid?
                            (complement @visited*)
                            (comp (partial not= 9) (partial at coords)))
                (map v+ deltas (repeat point))))
  @visited*)

(let [max-x    (count (first coords))
      max-y    (count coords)
      valid?   (fn [[x y]]
                 (and (<= 0 x (dec max-x))
                      (<= 0 y (dec max-y))))
      low-points
      (for [y     (range max-y)
            x     (range max-x)
            :let  [v (at coords [x y])]
            :when (->> (map v+ deltas (repeat [x y]))
                       (filterv valid?)
                       (mapv (partial at coords))
                       (every? (partial < v)))]
        [x y])]
  [(reduce + (map (comp inc (partial at coords)) low-points))
   (->> low-points
       (map (fn [origin] (count (fill! coords valid? (volatile! #{}) origin))))
       (sort #(compare %2 %1))
       (take 3)
       (reduce *))])

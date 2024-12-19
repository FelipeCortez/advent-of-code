(require '[clojure.string :as str])
(import [java.util PriorityQueue])

(def walls
  (mapv (comp (partial mapv parse-long)
              reverse
              (partial re-seq #"\d+"))
        (str/split-lines (slurp "2024/18.in"))))

(def wall-set (set (take 1024 walls)))
(def max-coord 70)

(def start [0 0])
(def goal [max-coord max-coord])

(defn in-bounds? [[i j]]
  (and (<= 0 i max-coord)
       (<= 0 j max-coord)))

(defn p+ [p1 p2] (mapv + p1 p2))

(defn neighbors
  ([p] (neighbors p wall-set))
  ([p wall-set]
   (keep (fn [delta] (let [neighbor (p+ p delta)]
                       (when (and (not (wall-set neighbor))
                                  (in-bounds? neighbor))
                         neighbor)))
         [[0 1] [0 -1] [-1 0] [1 0]])))

(def dijkstra
  (loop [frontier (doto (PriorityQueue.) (.add [0 start]))
         came-from {} cost-so-far {start 0}]
    (if (empty? frontier)
      [came-from cost-so-far]
      (let [[cost current] (.poll frontier)
            [frontier came-from cost-so-far]
            (reduce (fn [[frontier came-from cost-so-far] neighbor]
                      (if (< (inc cost) (get cost-so-far neighbor ##Inf))
                        [(doto frontier (.add [(inc cost) neighbor]))
                         (assoc came-from neighbor current)
                         (assoc cost-so-far neighbor (inc cost))]
                        [frontier came-from cost-so-far]))
                    [frontier came-from cost-so-far]
                    (neighbors current))]
        (recur frontier came-from cost-so-far)))))

(def came-from (first dijkstra))
(dec (count (take-while identity (iterate came-from goal))))

(defn flood [wall-set]
  (loop [visited #{}, to-visit [start]]
    (if (empty? to-visit)
      visited
      (let [current (peek to-visit)]
        (recur (conj visited current)
               (into (pop to-visit)
                     (remove visited (neighbors current wall-set))))))))

(last
 (take-while (comp not last)
             (map (fn [n-]
                    (let [n-walls (take (inc n-) walls)]
                      [(reverse (get walls n-))
                        n-walls
                       (contains? (flood (set n-walls)) goal)]))
                  (reverse (range (count walls))))))

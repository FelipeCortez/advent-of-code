(require '[clojure.string :as str]
         '[clojure.set :as set])

(def the-map (str/split-lines (slurp "2024/12.in")))
(def unvisited
  (set
   (for [i (range (count the-map)), j (range (count the-map))]
     [i j])))

(def deltas [[0 1] [0 -1] [1 0] [-1 0]])

(defn flood [unvisited position]
  (loop [to-visit [position], unvisited unvisited, visited #{}]
    (if (empty? to-visit)
      visited
      (let [visiting (peek to-visit)
            to-visit (pop to-visit)]
        (recur (into to-visit
                     (keep (fn [delta]
                             (let [neighbor (mapv + visiting delta)]
                               (when (and (contains? unvisited neighbor)
                                          (= (get-in the-map visiting)
                                             (get-in the-map neighbor)))
                                 neighbor)))
                           deltas))
               (disj unvisited visiting)
               (conj visited   visiting))))))

(def regions
  (loop [unvisited unvisited, regions []]
    (if (empty? unvisited)
      regions
      (let [plant (first unvisited), plant-region (flood unvisited plant)]
        (recur (set/difference unvisited plant-region)
               (conj regions plant-region))))))

(defn perimeter [region]
  (reduce (fn [s position]
            (into s (keep (fn [delta]
                            (let [neighbor (mapv + position delta)]
                              (when-not (contains? region neighbor)
                                (mapv + position (mapv (partial * 1/4) delta)))))
                          deltas)))
          #{}
          region))

;; part 1
(time
 (reduce + (map (fn [region] (* (count region) (count (perimeter region)))) regions)))

(defn side->orientation [[i _j]] (if (= (type i) clojure.lang.Ratio) :v :h))

(defn side-from-pos [start perimeter]
  (if (= (side->orientation start) :h)
    (into #{} cat [(take-while #(contains? perimeter %) (iterate #(update % 0 inc) start))
                   (take-while #(contains? perimeter %) (iterate #(update % 0 dec) start))])
    (into #{} cat [(take-while #(contains? perimeter %) (iterate #(update % 1 inc) start))
                   (take-while #(contains? perimeter %) (iterate #(update % 1 dec) start))])))

(defn sides [perimeter]
  (loop [unvisited perimeter, sides #{}]
    (if (empty? unvisited)
      sides
      (let [start (first unvisited), side (side-from-pos start perimeter)]
        (recur (set/difference unvisited side)
               (conj sides side))))))

;; part 2
(time
 (reduce + (map (fn [region] (* (count region)
                                (count (sides (perimeter region))))) regions)))

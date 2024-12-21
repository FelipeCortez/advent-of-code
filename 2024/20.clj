(require '[clojure.string :as str])

(def the-map (str/split-lines (slurp "2024/20.in")))

(let [*walls (transient #{})]
  (doseq [i (range (count the-map)), j (range (count the-map))
          :let [entity (get-in the-map [i j])]]
    (case entity
      \# (conj! *walls [i j])
      \S (def start [i j])
      \E (def goal [i j])
      \. nil))
  (def walls (persistent! *walls)))

(def p+ (partial mapv +))

(defn in-bounds? [[i j]]
  (and (<= 0 i (dec (count the-map)))
       (<= 0 j (dec (count the-map)))))

(defn neighbors [position visited]
  (into []
        (comp (map (partial p+ position))
              (filter in-bounds?)
              (remove (partial contains? walls))
              (remove (partial contains? visited)))
        [[0 1] [0 -1] [1 0] [-1 0]]))

(defn with [coll n] (mapv vector coll (repeat 4 n)))

(loop [to-visit [[goal 0]], goal-distance {}, visited #{}]
  (if (empty? to-visit)
    (def goal-distance goal-distance)
    (let [[position current-distance] (peek to-visit)]
      (recur (into (pop to-visit)
                   (with (neighbors position visited) (inc current-distance)))
             (assoc goal-distance position current-distance)
             (conj visited position)))))

(defn wall-neighbors [position visited]
  (into #{}
        (comp (map (partial p+ position))
              (remove visited)
              (filter in-bounds?)
              (filter (partial contains? walls)))
        [[0 1] [0 -1] [1 0] [-1 0]]))

(defn all-neighbors [position visited]
  (into #{} cat [(wall-neighbors position visited)
                 (neighbors position visited)]))

(defn visit-skipping [position]
  (loop [to-visit [[position 2]], visited #{}, distances []]
    (if (empty? to-visit)
      distances
      (let [[visiting steps-remaining] (peek to-visit)]
        (case steps-remaining
          2 (recur (into (pop to-visit) (with (wall-neighbors visiting visited) 1))
                   (conj visited visiting)
                   distances)

          1 (recur (into (pop to-visit)
                         (with (all-neighbors visiting visited) 0))
                   (conj visited visiting)
                   distances)

          0 (recur (pop to-visit)
                   (conj visited visiting)
                   (if (walls visiting)
                     distances
                     (conj distances (- (goal-distance position)
                                        (+ 2 (goal-distance visiting)))))))))))


(->> goal-distance
     keys
     (mapcat visit-skipping)
     (filter #(>= % 100))
     (count))

(defn manhattan [[i1 j1] [i2 j2]]
  (+ (abs (- i1 i2))
     (abs (- j1 j2))))

(defn visit-skipping' [position]
  (for [i (range -20 (inc 20)), j (range -20 (inc 20))
        :let [neighbor (p+ position [i j])]
        :when (and (in-bounds? neighbor)
                   (not (walls neighbor))
                   (goal-distance position)
                   (goal-distance neighbor)
                   (<= (manhattan position neighbor) 20))]
    (- (goal-distance position)
       (+ (manhattan position neighbor)
          (goal-distance neighbor)))))

(->> goal-distance
     keys
     (mapcat visit-skipping')
     (filter #(>= % 100))
     (count))

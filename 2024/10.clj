(require '[clojure.string :as str])

(defn char->int [c] (- (int c) (int \0)))
(def line->ints (partial mapv char->int))
(def the-map
  (mapv line->ints (str/split-lines (slurp "2024/10.in"))))

(def trailheads
  (for [i (range (count the-map)), j (range (count the-map))
        :when (= (get-in the-map [i j]) 0)]
    [i j]))

(defn p+ [p1 p2] (mapv + p1 p2))

(defn trails [position]
  (into #{}
        (if (= (get-in the-map position) 9)
          [position]
          (mapcat (fn [delta]
                    (let [neighbor (p+ position delta)]
                      (when (= (get-in the-map neighbor) (inc (get-in the-map position)))
                        (trails neighbor))))
                  [[0 1] [0 -1] [-1 0] [1 0]]))))

(defn rating [position]
  (if (= (get-in the-map position) 9)
    1
    (reduce + (map (fn [delta]
                      (let [neighbor (p+ position delta)]
                        (if (= (get-in the-map neighbor)
                               (inc (get-in the-map position)))
                          (rating neighbor)
                          0)))
                    [[0 1] [0 -1] [-1 0] [1 0]]))))

(reduce + (map (comp count trails) trailheads))
(reduce + (map rating trailheads))

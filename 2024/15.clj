(require '[clojure.string :as str]
         '[clojure.set :as set])

(def lines (str/split-lines (slurp "2024/15.sample2.in")))
(def the-map  (pop (pop lines)))
(def moves (peek lines))

(def pos->entity
  (into {}
        (for [i (range (count the-map))
              j (range (count (first the-map)))
              :let [entity (get-in the-map [i j])]
              :when (#{\# \@ \O} entity)]
          (if (= entity \@) (do (def initial-position [i j]) nil)
              [[i j] entity]))))

(def direction->delta {\< [0 -1], \> [0 1], \^ [-1 0], \v [1 0]})
(defn p+ [p1 p2] (mapv + p1 p2))

(defn debug [[position pos->entity]]
  (doseq [i (range (count the-map))
          j (range (count (first the-map)))]
    (when (= j 0) (println))
    (if (= [i j] position)
      (print \@)
      (print (or (get pos->entity [i j]) ".")))))

(defn move-and-push [[position pos->entity] direction]
  ;; (debug [position pos->entity])
  (let [delta (direction->delta direction)]
    (loop [looking-at (p+ position delta), seen-box false]
      (case (get pos->entity looking-at)
        \O (recur (p+ looking-at delta) true)
        \# [position pos->entity]
        [(p+ position delta)
         (if seen-box
           (-> pos->entity
               (dissoc (p+ position delta))
               (assoc looking-at \O))
           pos->entity)]))))

(defn gps-sum [[_ pos->entity]]
  (reduce + (keep (fn [[[i j] c]] (when (= c \O) (+ (* 100 i) j))) pos->entity)))

(gps-sum (reduce move-and-push [initial-position pos->entity] moves))

(defn box-at [boxes [i j]] (or (boxes [i j]) (boxes [i (dec j)])))

(let [*walls (transient #{}), *boxes (transient #{})]
  (doseq [i (range (count the-map))
        j (range (count (first the-map)))
        :let [entity (get-in the-map [i j])]
        :when (#{\# \@ \O} entity)]
    (case entity
      \O (conj! *boxes [i (* j 2)])
      \# (-> *walls
             (conj! [i (* j 2)])
             (conj! [i (inc (* j 2))]))
      \@ (def initial-position [i (* j 2)])))
  (def walls (persistent! *walls))
  (def boxes (persistent! *boxes)))

(defn debug' [[position boxes]]
  (doseq [i (range (count the-map))
          j (range (* (count (first the-map)) 2))]
    (when (= j 0) (println))
    (cond
      (= [i j] (box-at boxes [i j]))
      (print \[)

      (= [i (dec j)] (box-at boxes [i j]))
      (print \])

      (walls [i j])
      (print "#")

      (= [i j] position)
      (print "@")

      :else
      (print " "))))

(defn move-and-push' [[position boxes] direction]
  (debug' [position boxes])
  (let [delta (direction->delta direction)
        front (p+ position delta)]
    (loop [positions-to-check #{front}
           boxes-seen #{}
           visited #{}]
      (let [checking-position (first positions-to-check)
            box-at-position (some->> checking-position (box-at boxes))
            visit-unvisited
            (fn [positions-to-check position]
              (if ((conj visited checking-position) position)
                positions-to-check
                (conj positions-to-check position)))]
        (cond
          (nil? checking-position)
          [front
           (-> boxes
               (set/difference boxes-seen)
               (into (map (partial p+ delta) boxes-seen)))]

          (contains? walls checking-position)
          [position boxes]

          box-at-position
          (recur (-> positions-to-check
                     (disj checking-position)
                     (visit-unvisited box-at-position)
                     (visit-unvisited (p+ [0 1] box-at-position))
                     (visit-unvisited (p+ delta checking-position)))
                 (cond-> boxes-seen box-at-position (conj box-at-position))
                 (conj visited checking-position))

          :else
          (recur (disj positions-to-check checking-position)
                 boxes-seen
                 (conj visited checking-position)))))))

(defn gps-sum' [[_ boxes]]
  (reduce + (map (fn [[i j]] (+ (* 100 i) j)) boxes)))

(gps-sum' (reduce move-and-push' [initial-position boxes] moves))

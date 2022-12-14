(ns regolith-reservoir
  (:require [clojure.string :as str]))

(defn m+ [pos1 pos2]
  (mapv + pos1 pos2))

(defn draw [grid]
  (->> (for [j (range 14)
             i (range 489 509)]
         (if (grid [i j]) "#" "."))
       (partition 20)
       (map #(apply str %))))

(defn rangei [start end] (range start (inc end)))

(defn lineset [[i1 j1 :as _from] [i2 j2 :as _to]]
  (into #{}
        (if (= i1 i2)
          (map #(do [i1 %])
               (apply rangei (sort [j1 j2])))
          (map #(do [% j1])
               (apply rangei (sort [i1 i2]))))))

(defn simulate [grid]
  (let [x->lower-bound
        (->> (set (map first grid))
             (map (fn [i] [i (->> grid
                                  (filter #(= (first %) i))
                                  (map second)
                                  (apply max))]))
             (into {}))]
    (loop [grid grid, sand [500 0], sand-count 0]
      (let [lower-bound (x->lower-bound (first sand))]
        (cond
          (or (not lower-bound)
              (>= (second sand) lower-bound)) sand-count

          (not (grid (m+ sand [0 1])))  (recur grid (m+ sand [0 1]) sand-count)
          (not (grid (m+ sand [-1 1]))) (recur grid (m+ sand [-1 1]) sand-count)
          (not (grid (m+ sand [1 1])))  (recur grid (m+ sand [1 1]) sand-count)
          :else (recur (conj grid sand) [500 0] (inc sand-count)))))))

(defn simulate2 [grid]
  (let [floor (->> (map second grid) (apply max) (+ 2))
        collides? (fn [grid pos] (or (= (second pos) floor)
                                     (grid pos)))]
    (loop [grid grid, sand [500 0], sand-count 0]
      (cond
        (not (collides? grid (m+ sand [0 1])))  (recur grid (m+ sand [0 1]) sand-count)
        (not (collides? grid (m+ sand [-1 1]))) (recur grid (m+ sand [-1 1]) sand-count)
        (not (collides? grid (m+ sand [1 1])))  (recur grid (m+ sand [1 1]) sand-count)
        (= sand [500 0])              (inc sand-count)
        :else                         (recur (conj grid sand) [500 0] (inc sand-count))))))

(let [sand-source [500 0]]
  (->> (slurp "2022/14.in")
       (str/split-lines)
       (mapcat (comp (partial mapcat #(apply lineset %))
               (partial partition 2 1)
                     (partial partition 2)
                     (partial map parse-long)
                     #(re-seq #"\d+" %)))
       (set)
       ((juxt simulate simulate2))))

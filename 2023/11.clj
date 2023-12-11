(ns cosmic-expansion
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(defn shortest-path-length [[j1 i1] [j2 i2]]
  (+ (abs (- j1 j2)) (abs (- i1 i2))))

(defn expand-dimension [factor coll]
  (let [prev  (volatile! (first coll))
        shift (volatile! 0)]
    (reduce (fn [shifted x]
              (let [gap (* (dec factor) (max (dec (- x @prev)) 0))
                    real-x (+ x @shift gap)]
                (vreset! shift (+ @shift gap))
                (vreset! prev x)
                (conj shifted real-x)))
            []
            coll)))

(defn map-dimension [factor coll]
  (into {} (map vector coll (expand-dimension factor coll))))

(defn distances [factor]
  (let [grid
        (mapv vec (str/split-lines (slurp "2023/11.in")))

        galaxies
        (for [j (range (count grid))
              i (range (count (first grid)))
              :when (= \# (get-in grid [j i]))]
          [j i])

        j->real-j (map-dimension factor (sort (map first galaxies)))
        i->real-i (map-dimension factor (sort (map second galaxies)))

        real-galaxies
        (mapv (fn [[j i]] [(j->real-j j) (i->real-i i)]) galaxies)]
    (reduce +
            (for [g1 (range (count real-galaxies)),
                  g2 (range (inc g1) (count real-galaxies))]
              (shortest-path-length (nth real-galaxies g1) (nth real-galaxies g2))))))

(map distances [2 1000000])

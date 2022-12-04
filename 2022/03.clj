(ns rucksack-reorganization
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn split-at-middle [coll]
  (split-at (/ (count coll) 2) coll))

(defn in-common [[a b]]
  (set/intersection (set a) (set b)))

(def ->priority
  (merge (zipmap (map char (range (int \A) (inc (int \Z))))
                 (iterate inc 27))
         (zipmap (map char (range (int \a) (inc (int \z))))
                 (iterate inc 1))))

(->> (slurp "03.in")
     (str/split-lines)
     (map (comp ->priority
                first
                in-common
                split-at-middle))
     (reduce +))

(->> (slurp "03.in")
     (str/split-lines)
     (map set)
     (partition 3)
     (map (comp ->priority
                first
                (partial apply set/intersection)))
     (reduce +))

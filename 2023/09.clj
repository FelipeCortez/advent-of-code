(ns mirage-maintenance
  (:require [clojure.string :as str]))

(defn differences [xs]
  (->> xs
       (partition 2 1)
       (mapv #(- (second %) (first %)))))

(defn pyramidize [xs]
  (->> (iterate differences xs)
       (take-while #(not (= #{0} (set %))))
       (into [])))

(defn extrapolate [pyramid] (reduce + (mapv peek pyramid)))

(defn parse-line [line] (mapv parse-long (str/split line #"\s")))

(->> (slurp "2023/09.in")
     (str/split-lines)
     (mapv parse-line)
     (mapv (comp extrapolate pyramidize))
     (reduce +))

;; part 2

(->> (slurp "2023/09.in")
     (str/split-lines)
     (mapv parse-line)
     (mapv (comp extrapolate pyramidize vec reverse))
     (reduce +))

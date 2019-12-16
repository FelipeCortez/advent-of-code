(ns n-body-problem
  (:require [clojure.string :as str]))

(def moons (->> (slurp "12.in")
                str/split-lines
                (mapv (fn [line] {:velocity [0 0 0]
                                  :position (mapv read-string (re-seq #"-?\d+" line))}))))

(defn update-velocities [moons]
  (reduce (fn [coll moon]
            (let [new-vel (mapv + (:velocity moon) (velocity-deltas (:position moon) (map :position moons)))]
              (conj coll {:velocity new-vel :position (mapv + (:position moon) new-vel)})))
          []
          moons))

(defn attract [this other]
  (cond (= this other) 0
        (< this other) 1
        (> this other) -1))

(defn velocity-deltas [moon moons]
  (apply mapv (fn [this & others]
               (reduce (fn [sum current] (+ sum (attract this current))) 0 others))
         moon
         moons))

(defn energy [moons]
  (reduce (fn [sum {:keys [position velocity]}]
            (+ sum (* (reduce + (mapv #(Math/abs %) position))
                      (reduce + (mapv #(Math/abs %) velocity)))))
          0
          moons))

(energy (last (take 1001 (iterate update-velocities moons))))

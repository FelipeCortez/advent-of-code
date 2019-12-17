(ns n-body-problem
  (:require [clojure.string :as str]))

(def moons (->> (slurp "12.in")
                str/split-lines
                (mapv (fn [line] {:velocity [0 0 0]
                                  :position (mapv read-string (re-seq #"-?\d+" line))}))))

(defn attract [this other]
  (cond (= this other) 0
        (< this other) 1
        (> this other) -1))

(defn velocity-deltas [moon moons]
  (apply mapv (fn [this & others]
                (reduce (fn [sum current] (+ sum (attract this current))) 0 others))
         moon
         moons))

(defn update-velocities [moons]
  (reduce (fn [coll moon]
            (let [new-vel (mapv + (:velocity moon) (velocity-deltas (:position moon) (map :position moons)))]
              (conj coll {:velocity new-vel :position (mapv + (:position moon) new-vel)})))
          []
          moons))

(defn energy [moons]
  (reduce (fn [sum {:keys [position velocity]}]
            (+ sum (* (reduce + (mapv #(Math/abs %) position))
                      (reduce + (mapv #(Math/abs %) velocity)))))
          0
          moons))

;;; part 1
(energy (last (take 1001 (iterate update-velocities moons))))

;;; part 2
(defn just-dimension [moons idx]
  (mapv (fn [moon]
          (-> moon
              (update :position #(nth % idx))
              (update :velocity #(nth % idx)))) moons))

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))
(defn lcmv [& v] (reduce lcm v))

(apply lcmv
       (map (fn [idx]
              (loop [moons* moons
                     iteration 0]
                (if (and (not= iteration 0) (= (just-dimension moons idx) (just-dimension moons* idx)))
                  iteration
                  (recur (update-velocities moons*) (inc iteration)))))
            [0 1 2]))


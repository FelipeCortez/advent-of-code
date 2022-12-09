(ns rope-bridge
  (:require [clojure.string :as str]))

(def deltas {\U [0 1], \D [0 -1], \L [-1 0], \R [1 0]})

(defn step [delta [head & tail]]
  (let [head (mapv + head delta)]
    (:knots
     (reduce
      (fn [{:keys [knots prev-knot]} knot]
        (let [norm-delta (mapv (comp #(quot % 2) -)
                               prev-knot knot)
              new-knot (if (some (complement zero?) norm-delta)
                         (mapv - prev-knot norm-delta)
                         knot)]
          {:knots (conj knots new-knot)
           :prev-knot new-knot}))
      {:knots [head]
       :prev-knot head}
      tail))))

(defn simulate [snake-size]
  (->> (slurp "09.in")
       (str/split-lines)
       (reduce (fn [step-by-step [direction _ & steps]]
                 (apply conj step-by-step
                        (repeat (->> steps (apply str) parse-long)
                                (get deltas direction))))
               [])
       (reduce (fn [{:keys [visited positions]} delta]
                 (let [new-positions (step delta positions)]
                   {:positions new-positions
                    :visited (conj visited (peek new-positions))}))
               {:visited #{[0 0]}
                :positions (repeat snake-size [0 0])})
       (:visited) count))

(map simulate [2 10])

;; 2...
;; 1...
;; 0...
;;  012

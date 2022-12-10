(ns cathode-ray-tube
  (:require [clojure.string :as str]))


(let [simulation
      (->> (slurp "10.in")
           (str/split-lines)
           (reduce (fn [coll line]
                     (let [[op val] (str/split line #"\ ")
                           val (some-> val parse-long)
                           x (peek coll)]
                       (case op
                         "noop" (conj coll x)
                         "addx" (conj coll x (+ x val)))))
                   [1]))]
  [(reduce (fn [sum cycle]
             (+ sum (* cycle (get simulation (dec cycle)))))
           0
           [20 60 100 140 180 220])
   (->> (reduce (fn [display cycle]
                  (let [x (get simulation cycle)]
                    (conj display
                          (if (<= (dec x) (mod cycle 40) (+ x 2))
                            "|"
                            " "))))
                []
                (range 240))
        (partition 40)
        (map (partial apply str)))])

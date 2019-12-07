(ns universal-orbit-map
  (:require [clojure.string :as str]))

(def orbit-map (->> (slurp "06.in")
                    (str/split-lines)
                    (map (fn [s] (map keyword (str/split s #"\)"))))
                    (reduce (fn [m [orbited orbits]] (assoc m orbits orbited))
                            {})))

(defn orbits-number [m object]
  (loop [object object,number 0]
    (if (object m)
      (recur (object m) (inc number))
      number)))

(let [objects (set (keys orbit-map))]
  (apply + (map (partial orbits-number orbit-map) objects)))

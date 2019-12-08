(ns space-image-format
  (:require [clojure.string :as str]))

(defn to-int [char] (Integer/parseInt (str char)))

(def input (-> (slurp "08.in") str/trim))

(->> input
     (map to-int)
     (partition (* 25 6))
     (map frequencies)
     (apply min-key (fn [m] (get m 0)))
     ((fn [m] (* (get m 1) (get m 2)))))


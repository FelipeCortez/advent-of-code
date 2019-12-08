(ns space-image-format
  (:require [clojure.string :as str]))

(defn to-int [char] (Integer/parseInt (str char)))

(def input (-> (slurp "08.in") str/trim))

;;; part 1
(->> input
     (map to-int)
     (partition (* 25 6))
     (map frequencies)
     (apply min-key (fn [m] (get m 0)))
     ((fn [m] (* (get m 1) (get m 2)))))

;;; part 2
(->> input
     (map to-int)
     (partition (* 25 6))
     (apply map vector)
     (map #(reduce (fn [_ n] (when-not (= 2 n) (reduced n))) 0 %))
     (partition 25)
     (map (comp println #(str/replace % "1" "X") #(str/replace % "0" " ") str/join)))

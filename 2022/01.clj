(ns calorie-counting
  (:require [clojure.string :as str]))

(->> (slurp "01.in")
     (str/split-lines)
     (map #(when-not (clojure.string/blank? %) (Integer/parseInt %)))
     (partition-by nil?)
     (map #(reduce + %))
     (sort #(compare %2 %1))
     ((juxt identity (comp #(reduce + %) #(take 3 %)))))

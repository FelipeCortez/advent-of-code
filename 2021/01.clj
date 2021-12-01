(ns sonar-sweep)

(def example [199 200 208 210 200 207 240 269 260 263])

(def input (map read-string (clojure.string/split-lines (slurp "01.in"))))

(defn increases [coll] (count (filter #(apply < %) (partition 2 1 coll))))

;; part 1
(increases input)

;; part 2
(increases (map #(apply + %) (partition 3 1 input)))

(ns adapter-array
  (:require [clojure.string :as str]))

@(def input
   (map read-string (str/split-lines (slurp "10.input"))))

(defn diffs [coll]
  (let [coll (vec (conj (sort coll) 0))]
    (conj (mapv - (subvec coll 1) coll) 3)))

;; part 1
(apply * (vals (frequencies (diffs input))))

;; part 2
(apply max
       (vals (let [coll input
                   coll (conj (vec (sort coll)) (+ 3 (apply max coll)))]
               (loop [paths {0 1}, to-go coll]
                 (let [[fst] to-go]
                   (if (seq to-go)
                     (recur (assoc paths fst
                                   (apply + (map #(get paths (- fst %) 0)
                                                 [1 2 3])))
                            (rest to-go))
                     paths))))))

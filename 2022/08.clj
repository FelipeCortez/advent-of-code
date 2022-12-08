(ns treetop-tree-house
  (:require [clojure.string :as str]))

(defn upto
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (upto pred (rest s)))))))

((juxt #(count (filter :visible-from-outside? %))
       #(apply max (map :scenic-score %)))
 (let [grid
       (->> (slurp "08.in")
            (str/split-lines)
            (mapv #(mapv (comp parse-long str) %)))]
   (for [i (range (count grid))
         j (range (count grid))
         :let [height-at #(get-in grid %)
               height (height-at [j i])
               all-along [(map height-at (map #(do [j %]) (range (dec i) -1 -1)))
                          (map height-at (map #(do [j %]) (range (inc i) (count grid))))
                          (map height-at (map #(do [% i]) (range (dec j) -1 -1)))
                          (map height-at (map #(do [% i]) (range (inc j) (count grid))))]]]
     {:visible-from-outside?
      (some (fn [along] (every? #(< % height) along))
            all-along)

      :scenic-score
      (reduce *
              (map (fn [along] (count (upto #(>= % height) along)))
                   all-along))})))

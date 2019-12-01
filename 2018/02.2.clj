(ns advent.rightbox)

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn same-and-different [x y]
  (let [pairs (map vector x y)]
    (letfn [(alleq [pair] (apply = pair))]
      {:same      (->> (filter alleq pairs)
                       (map first)
                       (apply str))
       :different (remove alleq pairs)})))

(let [lines (clojure.string/split-lines (slurp "02.in"))]
  (println (->> (all-pairs lines)
                (map (partial apply same-and-different))
                (filter #(= 1 (count (:different %))))
                first
                :same)))

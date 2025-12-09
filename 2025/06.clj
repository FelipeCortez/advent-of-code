(require '[clojure.string :as str])

(def numbers+operations (str/split-lines (slurp "2025/06.in")))

(def transpose (partial apply mapv vector))

;; part 1
(defn parse-line [line]
  (mapv (fn [s] (or (parse-long s) (symbol s)))
        (str/split (str/trim line) #"[ ]+")))

(let [lines (mapv parse-line numbers+operations)]
  (reduce + (map eval (map list
                           (repeat 'reduce)
                           (peek lines)
                           (transpose (pop lines))))))

;; part 2
(def padded-lines
  (let [max-cols (apply max (map count numbers+operations))]
    (map (fn pad [line] (str line (apply str (repeat (- max-cols (count line)) \space))))
         numbers+operations)))

(defn mapvmapv [f coll] (mapv #(mapv f %) coll))

(def transposed-input (transpose padded-lines))

(defn lfirst [coll] (peek (first coll)))

(let [lines
      (into []
            (comp (partition-by #(every? (partial = \space) %))
                  (take-nth 2))
            transposed-input)]
  (reduce + (map eval (map list
                           (repeat 'reduce)
                           (mapv (comp symbol str lfirst) lines)
                           (mapvmapv (comp parse-long str/trim #(apply str %) pop) lines)))))

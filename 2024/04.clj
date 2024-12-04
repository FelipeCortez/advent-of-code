(require '[clojure.string :as str])
(def grid (str/split-lines (slurp "2024/04.in")))
(def size (count grid))
(defn in-bounds? [[i j]] (and (<= 0 i (dec size)) (<= 0 j (dec size))))
(defn bounded-iterate [f x] (into [] (take-while in-bounds? (iterate f x))))
(def e  (partial bounded-iterate (fn [[i j]] [i (inc j)])))
(def s  (partial bounded-iterate (fn [[i j]] [(inc i) j])))
(def sw (partial bounded-iterate (fn [[i j]] [(inc i) (inc j)])))
(def se (partial bounded-iterate (fn [[i j]] [(inc i) (dec j)])))
(def t-edge (e [0 0]))
(def l-edge (s [0 0]))
(def r-edge (s [0 (dec size)]))
(def tl-edges (concat t-edge l-edge))
(def tr-edges (concat t-edge r-edge))
(def lines
  (let [to (into #{} cat [(mapv sw tl-edges)
                          (mapv se tr-edges)
                          (mapv e l-edge)
                          (mapv s t-edge)])
        fro (mapv rseq to)]
    (into #{} cat [to fro])))

(defn char-at [pos] (get-in grid pos))
(def line->word #(apply str (map char-at %)))
(def count-xmas #(count (re-seq #"XMAS" %)))
(reduce + (map (comp count-xmas line->word) lines))

(count
 (for [i (range 1 (dec size))
       j (range 1 (dec size))
       :when (and (= (char-at [i j]) \A)
                  (= #{#{\M \S}}
                     (set [(set [(char-at [(inc i) (dec j)])
                                 (char-at [(dec i) (inc j)])])
                           (set [(char-at [(dec i) (dec j)])
                                 (char-at [(inc i) (inc j)])])])))]
   [i j]))

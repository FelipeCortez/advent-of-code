(ns binary-diagnostic)

(def example "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn binary [x] (Integer/parseInt x 2))

(defn pop-count [idx lines]
  (->> lines
       (map #(nth % idx))
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))))

(def astr #(apply str %))

(def lines (clojure.string/split-lines (slurp "03.in")))

;; part 1
(let [len     (count (first lines))
      gamma   (binary
               (astr
                (for [idx (range len)]
                  (->> lines
                       (pop-count idx)
                       (apply max-key second)
                       first))))
      epsilon (bit-xor gamma (binary (astr (repeat len 1))))]
  (* gamma epsilon))

;; part 2
(defn rating [tie-break prefer]
  (binary
   (loop [idx 0, remaining lines]
     (if (= 1 (count remaining))
       (first remaining)
       (let [bit (->> remaining (pop-count idx) (sort-by first tie-break) (apply prefer second) first)]
         (recur (inc idx)
                (filter (fn [s] (= bit (nth s idx))) remaining)))))))

(* (rating compare max-key)
   (rating #(compare %2 %1) min-key))

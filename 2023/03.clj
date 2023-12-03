(ns gear-ratios
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def vec->long (comp parse-long str/join))

(defn parse-line [y line]
  (loop [coll line, x 0, parts [], current-part {:yxs #{}, :num []}]
    (if coll
      (if (<= (int \0) (int (first coll)) (int \9))
        (recur (next coll)
               (inc x)
               parts
               (-> current-part
                   (update :num conj (first coll))
                   (update :yxs conj [y x])))
        (recur (next coll)
               (inc x)
               (if (seq (:yxs current-part))
                 (conj parts (update current-part :num vec->long))
                 parts)
               {:yxs #{}, :num []}))
      (cond-> parts
        (seq (:yxs current-part))
        (conj (update current-part :num vec->long))))))

(defn adjacencies [[y x]]
  (for [i [-1 0 1], j [-1 0 1]]
    [(+ y i) (+ x j)]))

;; part 1

(let [grid
      (->> (slurp "2023/03.in")
           (str/split-lines)
           (mapv vec))

      parts
      (remove (fn [{:keys [yxs]}]
                (= #{\.}
                   (set
                    (map (fn [yx] (get-in grid yx \.))
                         (set/difference
                          (reduce (fn [coll yx] (into coll (adjacencies yx))) #{} yxs)
                          yxs)))))
            (into [] (comp (map-indexed parse-line) cat) grid))]
  (reduce + (map :num parts)))

;; part 2
(let [grid (->> (slurp "2023/03.in")
                (str/split-lines)
                (mapv vec))]
  (->> grid
       (into [] (comp (map-indexed parse-line) cat))
       (mapcat (fn [{:keys [yxs num]}]
                 (map (fn [yx] [(get-in grid yx \.) yx num])
                      (set/difference
                       (reduce (fn [coll yx] (into coll (adjacencies yx))) #{} yxs)
                       yxs))))
       (filter (fn [[s]] (= s \*)))
       (group-by (fn [[a b c]] [a b]))
       (map second)
       (filter #(= 2 (count %)))
       (map #(apply * (map (fn [[_ _ x]] x) %)))
       (reduce +)))
;; => 81939900

(ns hill-climbing-algorithm
  (:require [clojure.string :as str])
  (:import [java.util PriorityQueue]))

(defn shortest [hmap start]
  (loop [m {:visited #{}
            :pq (doto (PriorityQueue.) (.add [0 start]))
            :node->distance {start 0}}]
    (let [{:keys [visited pq node->distance]} m

          [distance current-idx] (.poll pq)
          current (get-in hmap current-idx)

          neighbors
          (when current-idx
            (->> [[0 1] [0 -1] [-1 0] [1 0]]
                 (map #(mapv + current-idx %))
                 (filter (fn [neighbor-idx]
                           (let [neighbor (get-in hmap neighbor-idx)]
                             (and neighbor
                                  (not (visited neighbor-idx))
                                  (<= (int (cond (= \E neighbor) \z
                                                 (= \S neighbor) \a
                                                 :else neighbor))
                                      (inc (int current)))))))))]
      (cond
        (= \E current) distance
        (not current-idx) nil
        :else (recur
               (reduce (fn [{:keys [visited pq node->distance] :as m}
                            neighbor-idx]
                         (let [neighbor (node->distance neighbor-idx)]
                           (if (or (not neighbor)
                                   (< (inc distance) neighbor))
                             (-> m
                                 (assoc-in [:node->distance neighbor-idx] (inc distance))
                                 (update :pq (fn [pq] (doto pq
                                                        (.remove [neighbor neighbor-idx])
                                                        (.add [(inc distance) neighbor-idx])))))
                             m)))
                       (update m :visited conj current-idx)
                       neighbors))))))

(defn simulate []
  (let [hmap (->> (slurp "2022/12.in")
                  (str/split-lines)
                  (mapv #(mapv identity %)))
        start (first (for [i (range (count (first hmap)))
                            j (range (count hmap))
                            :when (= \S (get-in hmap [i j]))]
                        [i j]))
        hmap (assoc-in hmap start \a)
        as (for [i (range (count (first hmap)))
                 j (range (count hmap))
                 :when (= \a (get-in hmap [i j]))]
             [i j])]
    [(shortest hmap start)
     (apply min (filter identity (map (partial shortest hmap) as)))]))

(simulate)

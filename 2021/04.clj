(ns bingo
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(def example "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn transpose [vecs] (apply mapv vector vecs))

(let [[numbers & cards]
      (->> (slurp "04.in") str/split-lines (partition-by str/blank?) (remove (comp str/blank? first)))

      numbers
      (mapv read-string (str/split (first numbers) #","))

      winning-sets-v
      (mapv (fn [card]
              (mapv set
                    (into [] cat
                          ((juxt identity transpose)
                           (mapv (fn [line]
                                   (mapv read-string (str/split (str/trim line) #"\s+")))
                                 card)))))
            cards)

      winning-order*
      (atom [])]
  (reduce (fn [winning-sets-v n]
            (->> winning-sets-v
                 (mapv (fn [winning-sets]
                         (let [winning-sets2
                               (mapv (fn [s]
                                       (let [s2 (disj s n)]
                                         (if-not (seq s2)
                                           (let [unmarked-sum (apply + (disj (apply s/union winning-sets) n))
                                                 score        (* n unmarked-sum)]
                                             (swap! winning-order* conj score)
                                             nil)
                                           s2)))
                                     winning-sets)]
                           (when (every? identity winning-sets2)
                             winning-sets2))))
                 (filterv identity)))
          winning-sets-v
          numbers)
  ((juxt first last) @winning-order*))

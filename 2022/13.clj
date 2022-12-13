(ns distress-signal
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn compare-pair [coll1 coll2]
  (loop [coll1 coll1, coll2 coll2]
    (let [[x1 & rest1] coll1
          [x2 & rest2] coll2]
      (cond
        (and (not (seq coll1))
             (not (seq coll2)))
        0

        (not (seq coll1))
        -1

        (not (seq coll2))
        1

        (and (number? x1) (number? x2))
        (if (= x1 x2)
          (recur rest1 rest2)
          (compare x1 x2))

        (and (sequential? x1) (number? x2))
        (recur x1 (list x2))

        (and (number? x1) (sequential? x2))
        (recur (list x1) x2)

        (and (sequential? x1) (sequential? x2))
        (case (compare-pair x1 x2)
          0 (recur rest1 rest2)
          -1 -1
          1 1)))))

(->> (slurp "2022/13.in")
     (str/split-lines)
     (partition 2 3)
     (map (partial map read-string))
     ((juxt #(->> %
                  (keep-indexed (fn [idx pair]
                                  (when (neg? (apply compare-pair pair))
                                    (inc idx))))
                  (reduce +))
            #(->> %
                  (into [] cat)
                  (concat [[[2]] [[6]]])
                  (sort compare-pair)
                  (keep-indexed (fn [idx val]
                                  (when (#{[[2]] [[6]]} val)
                                    (inc idx))))
                  (reduce *)))))

(deftest compare-pairs-test
  (is (compare-pair [[1,1,3,1,1]
                     [1,1,5,1,1]]))

  (is (compare-pair [[[1], [2,3,4]]
                     [[1], 4]]))

  (is (not (compare-pair [[9]
                          [[8,7,6]]])))

  (is (compare-pair [[[4,4],4,4]
                     [[4,4],4,4,4]]))

  (is (not (compare-pair [[7,7,7,7]
                          [7,7,7]])))

  (is (compare-pair [[]
                     [3]]))

  (is (not (compare-pair [[[]]
                          []])))

  (is (not (compare-pair [[1,[2,[3,[4,[5,6,7]]]],8,9]
                          [1,[2,[3,[4,[5,6,0]]]],8,9]]))))

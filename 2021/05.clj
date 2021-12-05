(ns hydrothermal-adventure)

(def example "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

@(def lines (map (fn [line]
       (->> line
            (re-seq #"(\d+),(\d+) -> (\d+),(\d+)")
            first next (map read-string) (split-at 2)))
                 (clojure.string/split-lines (slurp "05.in"))))

(defn points-in-line [[[x1 y1] [x2 y2]]]
  (let [[x1 x2] (sort [x1 x2])
        [y1 y2] (sort [y1 y2])]
    (cond
      (= x1 x2)
      (map vector (repeat x1) (range y1 (inc y2)))

      (= y1 y2)
      (map vector (range x1 (inc x2)) (repeat y1)))))

(count (filter #(> % 1) (vals (frequencies (into [] cat (map points-in-line lines))))))

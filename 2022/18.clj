(ns boiling-boulders
  (:require [clojure.string :as str]))

(def cube->free-faces
  (->> (slurp "2022/18.in")
       (str/split-lines)
       (map #(do [(map parse-long (re-seq #"\d+" %)) 6]))
       (into {})))

(defn m+ [c1 c2] (map + c1 c2))

(defn neighbors [c]
  (map m+ [[1 0 0] [-1 0 0]
           [0 1 0] [0 -1 0]
           [0 0 1] [0 0 -1]]
       (repeat c) ))

(defn neighbor? [c1 c2]
  (->> (map #(Math/abs (- %1 %2)) c1 c2)
       (#{[0 0 1] [0 1 0] [1 0 0]})
       boolean))

(defn part-1 []
  (reduce (fn [cube->free-faces cube]
            (reduce (fn [cube->free-faces neighbor]
                      (if (get cube->free-faces neighbor)
                        (update cube->free-faces neighbor dec)
                        cube->free-faces))
                    cube->free-faces
                    (neighbors cube)))
          cube->free-faces
          (keys cube->free-faces)))

(reduce + (vals (part-1)))

;;

(defn third [coll] (first (next (next coll))))

(defn part-2 []
  (let [min-bounds [(dec (apply min (map first  (keys cube->free-faces))))
                    (dec (apply min (map second (keys cube->free-faces))))
                    (dec (apply min (map third  (keys cube->free-faces))))]

        max-bounds [(inc (apply max (map first  (keys cube->free-faces))))
                    (inc (apply max (map second (keys cube->free-faces))))
                    (inc (apply max (map third  (keys cube->free-faces))))]

        inside-bounds? (fn [[x y z]]
                         (let [[min-x min-y min-z] min-bounds
                               [max-x max-y max-z] max-bounds]
                           (and (<= min-x x max-x)
                                (<= min-y y max-y)
                                (<= min-z z max-z))))

        cube? (set (keys cube->free-faces))]

    (loop [to-visit #{min-bounds}
           visited #{min-bounds}
           sides #{}]
      (if (seq to-visit)
        (let [space (first to-visit)
              current-neighbors (neighbors space)
              more-to-visit (filter #(and (inside-bounds? %)
                                          (not (visited %))
                                          (not (cube? %)))
                                    current-neighbors)
              sides (reduce (fn [sides neighbor]
                              (if (cube? neighbor)
                                (conj sides [space neighbor])
                                sides))
                            sides
                            current-neighbors)
              to-visit (-> to-visit
                           (disj space)
                           (into more-to-visit))
              visited (conj visited space)]
          (recur to-visit visited sides))
        (count sides)))))

(part-2)

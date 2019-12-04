(ns crossed-wires
  (:require [clojure.string :as str]))

(def delta-ops {:R [:x +]
                :L [:x -]
                :U [:y +]
                :D [:y -]})

(def other-coord {:x :y, :y :x})

(defn split-commas [s] (str/split s #","))

(defn segment-str->segment [segment-str]
  {:dir   (keyword (subs segment-str 0 1))
   :delta (Integer/parseInt (subs segment-str 1))})

(def wires (->> (slurp "03.in")
                str/split-lines
                (mapv split-commas)))

(def wires-test "R8,U5,L5,D3\nU7,R6,D4,L4")

(defn relative->absolute [segments]
  (loop [pos      {:x 0 :y 0}
         segments segments
         result   {:h [], :v []}]
    (if-not (seq segments)
      result
      (let [segment   (first segments)
            [coord f] (delta-ops (:dir segment))
            new-pos (merge-with f pos {coord (:delta segment)})]
        (recur new-pos
               (rest segments)
               (update result (coord {:x :h, :y :v})
                       #(conj % [(pos (other-coord coord)) (map coord [pos new-pos])])))))))

(defn crossings [[] wire2] )

(apply merge-with concat
       (mapv (fn [wire] (relative->absolute (mapv segment-str->segment wire)))
             (mapv split-commas (str/split-lines wires-test))))

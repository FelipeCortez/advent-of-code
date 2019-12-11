(ns monitoring-station
  (:require [clojure.string :as str]))

(defn read-asteroids [s]
  (-> s
      str/trim-newline
      str/split-lines))

(def um
".#..#
.....
#####
....#
...##")

(defn get-xy [m x y] (get-in m [y x]))

(def asteroid-map
  (let [asteroids (read-asteroids um), h (count asteroids), w (count (first asteroids))]
    {:h h, :w w,:asteroids (into (sorted-set)
                                 (for [y (range h), x (range w), :when (= \# (get-xy asteroids x y))]
                                   [x y]))}))

(defn count-visible [{:keys [h w asteroids]} [ax ay]]
  (let [relative-asteroids (into (sorted-set) (map (fn [[x y]] [(- x ax) (- y ay)]) asteroids))]
    relative-asteroids))

(defn line-of-sight [{:keys [h w]} [abs-ax abs-ay] [abs-x abs-y]]
  (let [rel-pos [(- abs-x abs-ax) (- abs-y abs-ay)]
        rel-gcd (gcd (first rel-pos) (second rel-pos))
        rel-increment (map #(/ % rel-gcd) rel-pos)
        [from-x to-x] (map (partial + (- abs-x abs-ax)) [0 (dec w)])
        [from-y to-y] (map (partial + (- abs-y abs-ay)) [0 (dec h)])]
    (println rel-pos)
    (println rel-gcd)
    (println rel-increment)
    #_(take-while (fn [[x y]] (and (<= from-x x to-x) (<= from-y y to-y)))
                (iterate (fn [pos] (mapv + rel-pos pos)) rel-pos))))

(defn abs [n] (max n (- n)))

(defn gcd [a b]
  (loop [a (abs a) b (abs b)]
    (if (zero? b) a,
        (recur b (mod a b)))))

(line-of-sight {:h 10 :w 10} [3 3] [1 2])

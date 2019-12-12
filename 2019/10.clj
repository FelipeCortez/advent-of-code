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

(defn abs [n] (max n (- n)))

(defn gcd [a b]
  (loop [a (abs a) b (abs b)]
    (if (zero? b) a,
        (recur b (mod a b)))))


(defn blocked-positions [{:keys [h w]} [abs-ax abs-ay] [abs-x abs-y]]
  (let [rel-pos [(- abs-x abs-ax) (- abs-y abs-ay)]
        rel-gcd (gcd (first rel-pos) (second rel-pos))
        rel-increment (map #(/ % rel-gcd) rel-pos)]
    (rest (take-while (fn [[x y]] (and (<= 0 x (dec w)) (<= 0 y (dec h))))
                      (iterate (fn [pos] (mapv + pos rel-increment)) [abs-x abs-y])))))

(defn count-visible [{:keys [h w asteroids] :as asteroid-map} asteroid]
  (let [other-asteroids (disj (set asteroids) asteroid)]
    (count (clojure.set/difference
            other-asteroids
            (reduce (fn [s other-asteroid] (apply conj s (blocked-positions asteroid-map asteroid other-asteroid)))
                    #{}
                    other-asteroids)))))

(count-visible asteroid-map [1 0])

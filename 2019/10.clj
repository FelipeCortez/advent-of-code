(ns monitoring-station
  (:require [clojure.string :as str]))

(defn read-asteroids [s]
  (-> s
      str/trim-newline
      str/split-lines))

(def input (slurp "10.in"))

(defn get-xy [m x y] (get-in m [y x]))

(def asteroid-map
  (let [asteroids (read-asteroids input), h (count asteroids), w (count (first asteroids))]
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

(defn visible [{:keys [h w asteroids] :as asteroid-map} asteroid]
  (let [other-asteroids (disj (set asteroids) asteroid)]
    (clojure.set/difference
     other-asteroids
     (reduce (fn [s other-asteroid] (apply conj s (blocked-positions asteroid-map asteroid other-asteroid)))
             #{}
             other-asteroids))))

;;; part 1
(apply max (map (fn [asteroid] (count (visible asteroid-map asteroid))) (:asteroids asteroid-map)))

;;; part 2
(defn angle [x y]
  "^: 0, >: 90, v: 180, <: 270"
  (let [deg (+ (- (Math/toDegrees (Math/atan2 y x))) 90)]
    (if (< deg 0)
      (+ 360 deg)
      deg)))

(let [[ax ay :as chosen-one] (apply max-key (fn [asteroid] (count (visible asteroid-map asteroid)))
                                    (:asteroids asteroid-map))]
  (loop [asteroid-map asteroid-map
         vaporized []]
    (if (= 1 (count (:asteroids asteroid-map)))
      (let [[x y] (nth vaporized (dec 200))] (+ (* 100 x) y))
      (let [now-vaporized (sort-by (fn [[x y]] (angle (- x ax) (- ay y)))
                                   (visible asteroid-map chosen-one))]
        (recur (update asteroid-map :asteroids #(apply disj % now-vaporized))
               (apply conj vaporized now-vaporized))))))

(ns beacon-exclusion-zone
  (:require [clojure.string :as str]))

(def i first)
(def j second)

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn beacon-row-range [sensor sensor-size row]
  (let [j-diff (- sensor-size (abs (- (j sensor) row)))]
    (when (>= j-diff 0)
      [(- (i sensor) j-diff)
       (+ (i sensor) j-diff)])))

(defn beacon-exclusion-zones [sensors+beacons for-row]
  (->> sensors+beacons
       (map (fn [[sensor beacon]]
              [sensor (manhattan sensor beacon)]))
       (map (fn [[sensor sensor-size]]
              (beacon-row-range sensor sensor-size for-row)))
       (filter identity)
       (sort)))

(defn impossible-beacon [sensors+beacons for-row]
  (let [row->beacon-at-col?
        (update-vals (group-by second (map second sensors+beacons))
                     (fn [beacons] (into #{} (map first beacons))))

        beacon-exclusion-zones
        (beacon-exclusion-zones sensors+beacons for-row)

        first-col (apply min (flatten beacon-exclusion-zones))
        last-col  (apply max (flatten beacon-exclusion-zones))]
    (count
     (filter (fn [col]
               (and (not (some-> (row->beacon-at-col? for-row)
                                 (contains? col))) ;; exclude this exclusion
                    (some (fn [[from to]] (<= from col to))
                          beacon-exclusion-zones)))
             (range first-col (inc last-col))))))

(defn coverage [beacon-exclusion-zones]
  (reduce (fn [last-to [from to]]
            (if (<= from (inc last-to))
              (max last-to to)
              (reduced (inc last-to))))
          0
          beacon-exclusion-zones))

(let [sensors+beacons
      (->> (slurp "2022/15.in")
           (str/split-lines)
           (map #(->> (re-seq #"-?\d+" %)
                      (map parse-long)
                      (partition 2))))]
  [(impossible-beacon sensors+beacons 2000000)
   (keep (fn [idx]
           (let [stopped-at (coverage* (beacon-exclusion-zones sensors+beacons idx))]
             (when (< stopped-at 4000000)
               [stopped-at idx (+ idx (* stopped-at 4000000))])))
         (range 4000000))])

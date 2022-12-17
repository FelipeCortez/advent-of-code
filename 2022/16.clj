(ns proboscidea-volcanium
  (:require [clojure.string :as str]))

(def valves
  (->> (slurp "2022/16.in")
       (str/split-lines)
       (map (fn [line]
              (let [[valve & neighbors] (map keyword (re-seq #"[A-Z]{2}" line))
                    flow (parse-long (re-find #"\d+" line))]
                [valve {:flow flow :neighbors neighbors}])))
       (into {})))

(defn other [s x] (first (disj s x)))

(def fromto->distance
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY :AA)
         distances {}
         visited #{}]
    (if-let [current (peek queue)]
      (let [neighbors (-> valves current :neighbors)]
        (recur
         (-> queue pop (into (filter (complement visited) neighbors)))
         (reduce (fn [distances neighbor]
                   (let [distances
                         (reduce (fn [distances distance-key]
                                   (let [the-other (other distance-key neighbor)]
                                     ;; #dbg ^{:break/when (#{:CC :EE} the-other)}
                                     the-other
                                     (if (or (= the-other current)
                                             (distances #{current the-other}))
                                       distances
                                       (assoc distances #{current the-other}
                                              (inc (get distances distance-key))))))
                                 distances
                                 (filter #(neighbor %) (keys distances)))]
                     (assoc distances #{current neighbor} 1)))
                 distances
                 neighbors)
         (conj visited current)))
      distances)))

(defn calculate-one [sequence]
  (let [one sequence]
    (loop [current :AA
           minutes 30
           to-visit sequence
           sum 0]
      (let [move-to (first to-visit)]
        (if (and move-to (pos? minutes))
          (let [distance (fromto->distance #{current move-to})
                waste (inc distance)
                minutes (- minutes waste)]
            (recur move-to
                   minutes
                   (next to-visit)
                   (+ sum (* minutes (-> valves move-to :flow)))))
          sum)))))

(defn points-available [sequence]
  (let [one sequence]
    (loop [current :AA
           minutes 30
           to-visit sequence
           sum 0]
      (let [move-to (first to-visit)]
        (cond
          (not (pos? minutes))
          0

          (not move-to)
          (->> (clojure.set/difference (set (keys valves))
                                       (set sequence))
               (map (fn [unopened-valve] (* (-> valves unopened-valve :flow) minutes)))
               (reduce +)
               (+ sum))

          :else
          (let [distance (fromto->distance #{current move-to})
                waste (inc distance)
                minutes (- minutes waste)]
            (recur move-to
                   minutes
                   (next to-visit)
                   (+ sum (* minutes (-> valves move-to :flow))))))))))

(defn explore [sequence s !max]
  (doseq [valve s]
    (let [points (calculate-one sequence)]
      (when (> points @!max)
        (reset! !max points)))
    (when (> (points-available sequence) @!max)
      (explore (conj sequence valve)
               (disj s valve)
               !max))))

(defn calculate-all []
  (let [!max (atom 0)]
    (explore []
             (into (sorted-set-by #(let [flow+alpha
                                         (fn [k]
                                           [(* (get-in valves [k :flow])
                                               (- 30
                                                  (fromto->distance #{k :AA})))
                                            k])]
                                     (compare (flow+alpha %2) (flow+alpha %1))))
                   (set (filter (fn [k] (pos? (:flow (get valves k))))
                                (keys valves))))
             !max))
  @!max)

(calculate-all)

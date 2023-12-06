(ns if-you-give-a-seed-a-fertilizer
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "2023/05.in")
       (str/split-lines)
       (partition-by str/blank?)
       (take-nth 2)))

(defn parse-nums [s] (map parse-long (re-seq #"\d+" s)))

(defn parse-mapping [[header & mappings]]
  (mapv (fn [line]
          (let [[dst src rng] (parse-nums line)]
            {:src src :dst dst :rng rng}))
        mappings))

(defn parse-in [[[seeds] & mappings]]
  {:seeds (parse-nums seeds)
   :mappings (mapv parse-mapping mappings)})

(defn transform-one [x mappings]
  (reduce (fn [x {:keys [src dst rng]}]
            (if (<= src x (+ src (dec rng)))
              (reduced (+ x (- dst src)))
              x))
          x
          mappings))

(defn transform-all [x]
  (reduce transform-one x (:mappings (parse-in input))))

(apply min (map transform-all (:seeds (parse-in input))))


;; part 2

(comment
  (def decimated-ranges
    (->> (partition 2 (:seeds (parse-in input)))
         (map vec)
         (map (fn [[f s]] (range f (+ f s) 1e5)))))

  (run! (fn [rng]
          (tap>
           ^{:portal.viewer/default :portal.viewer/line-chart}
           {:x rng
            :y (map transform-all rng)}))
        decimated-ranges)

  (apply min (map transform-all (range a b)))

  nil)

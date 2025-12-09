(require '[clojure.string :as str])

(def lines (str/split-lines (slurp "2025/07.in")))

(def start (.indexOf (first lines) (int \S)))

(def beam?
  (into #{}
        (for [i (range (count lines))
              j (range (count (first lines)))
              :when (= (get-in lines [i j]) \^)]
          [i j])))

;; part 1
(reduce (fn [[*splits beams] i]
          [*splits
           (->> beams
                (mapcat (fn [j] (if (beam? [i j]) (do (swap! *splits inc) [(inc j) (dec j)]) [j])))
                (set))])
        [(atom 0) #{start}]
        (range (count lines)))

;; part 2
(def count-journeys
  (memoize
   (fn [[i j]]
     (cond
       (= i (dec (count lines)))
       1

       (beam? [i j])
       (+ (count-journeys [i (dec j)])
          (count-journeys [i (inc j)]))

       :else
       (count-journeys [(inc i) j])))))

(count-journeys [0 start])
;; => 13418215871354

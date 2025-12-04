(require '[clojure.string :as str]
         '[clojure.set :as set])

(def lines (str/split-lines (slurp "2025/04.in")))

(def rolls
  (into #{}
        (for [i (range (count lines))
              j (range (count (first lines)))
              :when (= \@ (get-in lines [i j] ))]
          [i j])))

(def deltas (for [dx [-1 0 1] dy [-1 0 1] :when (not (= dx dy 0))] [dx dy]))

(def v+ (partial mapv +))

;; part 1
(defn accessible [rolls]
  (into #{}
        (filter (fn [pos] (< (count (keep (comp rolls (partial v+ pos))
                                          deltas))
                             4)))
        rolls))

(count (accessible rolls))

;; part 2
(loop [rolls rolls, removed 0]
  (let [accessible-rolls (accessible rolls)
        new-rolls (set/difference rolls accessible-rolls)]
    (if (empty? accessible-rolls)
      removed
      (recur new-rolls (+ removed (count accessible-rolls))))))

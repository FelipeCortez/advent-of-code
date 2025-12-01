(require '[clojure.string :as str])

(def rotations
  (->> (slurp "2025/01.in")
       (str/split-lines)
       (map (comp parse-long
                  #(str/replace % "L" "-")
                  #(str/replace % "R" "")))))

;; part 1
(reduce (fn [[dial zero-count] rotation]
          (let [dial (+ rotation dial)]
            [dial (if (zero? (mod dial 100))
                    (inc zero-count)
                    zero-count)]))
        [50 0]
        rotations)

;; part 2
(defn crossings [dial rotation]
  (let [op (if (neg? rotation) dec inc)]
    (count (filter (comp zero? #(mod % 100))
                   (take (abs rotation) (iterate op (op dial)))))))

(reduce (fn [[dial zero-count] rotation]
          [(+ dial rotation) (+ zero-count (crossings dial rotation))])
        [50 0]
        rotations)

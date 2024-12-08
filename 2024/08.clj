(require '[clojure.string :as str])

(def the-map (str/split-lines (slurp "2024/08.in")))

(defn in-bounds? [position] (every? #(<= 0 % (dec (count the-map))) position))
(defn p*2 [[i j]] [(* 2 i) (* 2 j)])
(defn p- [[i1 j1] [i2 j2]] [(- i1 i2) (- j1 j2)])

(def antenna-groups
  (reduce (fn [m [a i j]] (update m a (fnil conj []) [i j]))
          {}
          (for [i (range (count the-map)), j (range (count the-map))
                :let [antenna (get-in the-map [i j])]
                :when (not= \. antenna)]
            [antenna i j])))

(defn antinodes [antenna-group]
  (into [] (comp cat (filter in-bounds?))
        (for [a (range (count antenna-group)),
              b (range (inc a) (count antenna-group))
              :let [a (nth antenna-group a)
                    b (nth antenna-group b)]]
          [(p- (p*2 a) b)
           (p- (p*2 b) a)])))

(count
 (into #{} cat
       (vals (update-vals antenna-groups antinodes))))

;; ---

(defn p*n [[i j] n] [(* n i) (* n j)])
(defn p+ [[i1 j1] [i2 j2]] [(+ i1 i2) (+ j1 j2)])

(defn antinodes+harmonics [antenna-group]
  (into [] (comp cat (filter in-bounds?))
        (for [a (range (count antenna-group)),
              b (range (inc a) (count antenna-group))
              :let [a (nth antenna-group a)
                    b (nth antenna-group b)]]
          (concat
           (take-while in-bounds? (map #(p+ a (p*n (p- b a) %)) (range)))
           (take-while in-bounds? (map #(p+ b (p*n (p- a b) %)) (range)))))))

(count
 (into #{} cat
       (vals (update-vals antenna-groups antinodes+harmonics))))

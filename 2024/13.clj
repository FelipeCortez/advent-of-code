(require '[clojure.string :as str])

(def configurations
  (->> (slurp "2024/13.in")
       (str/split-lines)
       (map (fn [line] (mapv parse-long (re-seq #"\d+" line))))
       (partition 3 4)
       (mapv (partial into []))))

;; brute
(defn v<=v [[x1 y1] [x2 y2]] (and (<= x1 x2) (<= y1 y2)))
(defn v+v [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(defn s*v [s [x y]] [(* s x) (* s y)])

(defn play-one [a b prize]
  (let [as (take 100 (take-while #(v<=v % prize) (map s*v (range) (repeat a))))
        bs (take 100 (take-while #(v<=v % prize) (map s*v (range) (repeat b))))
        solutions
        (for [a (map-indexed vector as)
              b (map-indexed vector bs)
              :let [[a-idx a'] a, [b-idx b'] b]
              :when (= (v+v a' b') prize)]
          (+ (* 3 a-idx) b-idx))]
    (or (some->> (seq solutions) (apply min)) 0)))

(reduce + (map (partial apply play-one) configurations))

;; smart
(defn play-one [a b prize]
  (let [[bax bay] a, [bbx bby] b, [px py] prize
        b (/ (- (* py bax)  (* px bay))
             (- (* bax bby) (* bbx bay)))
        a (/ (- px (* b bbx))
             bax)]
    (if (and (zero? (mod a 1)) (zero? (mod b 1)))
      (+ (* 3 a) b)
      0)))

;; part 1
(reduce + (map (partial apply play-one) configurations))
;; part 2
(reduce + (map (partial apply play-one)
               (mapv (fn [[a b [px py]]]
                       [a b [(+ 10000000000000 px)
                             (+ 10000000000000 py)]])
                     configurations)))

(ns chocolate-charts)

(defn split-int [v] (->> v str vec (map #(Character/digit % 10))))

(println
 (let [x 890691]
   (loop [grades [3 7]
          a 0
          b 1]
     (if (= (count grades) (+ 10 x))
       (clojure.string/join (take-last 10 grades))
       (let [at-a (grades a)
             at-b (grades b)
             new-grades (apply conj grades (split-int (+ at-a at-b)))]
         (recur new-grades
                (mod (+ a (inc at-a)) (count new-grades))
                (mod (+ b (inc at-b)) (count new-grades))))))))

(println
 (let [x (str 890691)]
   (loop [grades [3 7]
          a 0
          b 1
          cnt 1]
     (if (and (> (count grades) (count x))
              (or (= x (clojure.string/join (subvec grades
                                                    (dec (- (count grades) (count x)))
                                                    (dec (count grades))))) ;; should dec the answer in this case
                  (= x (clojure.string/join (subvec grades (- (count grades) (count x)))))))
       (- (count grades) (count x))
       (let [at-a (grades a)
             at-b (grades b)
             new-grades (apply conj grades (split-int (+ at-a at-b)))]
         (recur new-grades
                (mod (+ a (inc at-a)) (count new-grades))
                (mod (+ b (inc at-b)) (count new-grades))
                (inc cnt)))))))

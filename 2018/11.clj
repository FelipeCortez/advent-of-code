(ns chronal-charge)

(def serial-number 1718)

(def fuel-cell
  (memoize (fn [x y sn]
             (let [rack-id (+ x 10)]
               (-> rack-id (* y) (+ sn) (* rack-id) (#(mod (int (/ % 100)) 10)) (- 5))))))

(defn largest-divisor [x]
  (if (zero? (mod x 2))
    (/ x 2)
    (/ x (or (first (filter #(zero? (mod x %))
                            (range 3 (-> x Math/sqrt int inc) 2)))
             x))))

(def fuel-square
  (memoize
   (fn [x y sn size]
     (let [step (largest-divisor size)]
       (if (> step 1)
         (apply + (for [xs (range x (+ x size) step)
                        ys (range y (+ y size) step)]
                    (fuel-square xs ys sn step)))
         (apply + (for [xs (range x (+ size x))
                        ys (range y (+ size y))]
                    (fuel-cell xs ys sn))))))))

(defn calculate-all [squares sn]
  (map (fn [square]
         (let [[x y size] square]
           (vector square (fuel-square x y sn size)))) squares))

(def max-size 30)

(println
 (apply max-key
        second
        (calculate-all (for [size (range 1 (inc max-size))
                             y    (range 1 (+ 2 (- 300 max-size)))
                             x    (range 1 (+ 2 (- 300 max-size)))]
                         [x y size])
                       serial-number)))

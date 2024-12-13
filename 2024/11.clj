(def count-stones
  (memoize
   (fn [n x]
     (if (zero? n)
       1
       (let [stone-length (count (str x))]
         (cond
           (zero? x)
           (count-stones (dec n) 1)

           (even? stone-length)
           (+ (count-stones (dec n) (parse-long (subs (str x) 0 (/ stone-length 2))))
              (count-stones (dec n) (parse-long (subs (str x) (/ stone-length 2) stone-length))))

           :else
           (count-stones (dec n) (* 2024 x))))))))

(def initial-stones (read-string (format "[%s]" (slurp "2024/11.in"))))
(reduce + (map (partial count-stones 25) initial-stones))
(reduce + (map (partial count-stones 75) initial-stones))

(ns the-treachery-of-whales)

(def example [16,1,2,0,4,2,7,1,2,14])
@(def input (map read-string (clojure.string/split (slurp "07.in") #",")))

(map (fn [f]
       (let [from (apply min input)
             to   (apply max input)]
         (apply min
                (for [position (range from (inc to))]
                  (reduce + (map #(f (Math/abs (- % position))) input))))))
     [identity #(/ (* % (inc %)) 2)])

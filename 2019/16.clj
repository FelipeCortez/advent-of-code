(ns fft
  (:require [clojure.string :as str]))

(def input (str/trim-newline (slurp "16.in")))

(defn pattern
  ([n] (drop 1 (pattern 1 n (cycle [0 1 0 -1]))))
  ([cnt n vals]
   (lazy-seq (cons (first vals)
                   (pattern (if (= cnt n) 1 (inc cnt))
                            n
                            (if (= cnt n) (rest vals) vals))))))

(defn char->int [c] (Integer/parseInt (str c)))

(defn tens [n] (Math/abs (char->int (last (str n)))))

(defn compute
  [input]
  (str/join (pmap (fn [idx]
                    (tens (reduce + (mapv (fn [input-digit pattern-digit]
                                            (* input-digit pattern-digit))
                                          (map char->int input)
                                          (pattern idx)))))
                  (range 1 (inc (count input))))))

(str/join (take 8 (last (take 101 (iterate compute input)))))

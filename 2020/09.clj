(ns encoding-error
  (:require [clojure.string :as str]))

@(def sample (mapv read-string
                 (str/split-lines (slurp "09.sample"))))

@(def input (mapv read-string
                  (str/split-lines (slurp "09.input"))))

;; part 1
(defn get-weak [cypher preamble-len]
  (loop [idx preamble-len]
         (let [window (take preamble-len (drop (- idx preamble-len) cypher))]
           (if (seq (for [x     (range preamble-len)
                          y     (range (inc x) preamble-len)
                          :when (= (nth cypher idx) (+ (nth window x) (nth window y)))]
                      true))
             (recur (inc idx))
             (nth cypher idx)))))

@(def not-the-sum-of-some-of-the-previous-25 (get-weak input 25))

;; part 2
(apply +
       (loop [idx 0, len 2]
         (let [contiguous-vals (subvec input idx (+ idx len))
               contiguous-sum  (apply + contiguous-vals)]
           (cond
             (= contiguous-sum not-the-sum-of-some-of-the-previous-25) ((juxt (partial apply min) (partial apply max)) contiguous-vals)
             (> contiguous-sum not-the-sum-of-some-of-the-previous-25) (recur (inc idx) 2)
             (> len (count input))                                     (recur (inc idx) 2)
             (< contiguous-sum not-the-sum-of-some-of-the-previous-25) (recur idx (inc len))))))

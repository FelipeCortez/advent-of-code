(ns supply-stacks
  (:require [clojure.string :as str]))

(defn nths [coll idxs]
  (into [] (map #(get coll %) idxs)))

(defn parse-instruction [line]
  (zipmap [:count :from :to]
          (map parse-long (re-seq #"[0-9]+" line))))

(defn read-stack [stacks n]
  (filterv (complement #{nil \space})
           (mapv #(get % n) stacks)))

(defn move-9000
  ([stacks from to]
   (let [stack-from (dec from)
         stack-to   (dec to)]
     (if-let [top (peek (get stacks stack-from))]
       (-> stacks
           (update stack-from pop)
           (update stack-to conj top))
       stacks)))
  ([stacks count from to]
   (nth (iterate #(move-9000 % from to) stacks) count)))

(defn vec-split-from-end [n v]
  (let [count (count v)
        at (max (- count n) 0)]
    [(subvec v 0 at) (subvec v at count)]))

(defn move-9001 [stacks count from to]
  (let [stack-from (dec from)
        stack-to   (dec to)
        [new-from to-move] (vec-split-from-end count (get stacks stack-from))]
    (-> stacks
        (assoc stack-from new-from)
        (assoc stack-to (apply conj (get stacks stack-to) to-move)))))

(defn simulate [move-fn]
  (let [[stacks instructions]
        (->> (slurp "05.in")
             (str/split-lines)
             (split-with (complement str/blank?)))

        stack-count
        (->> stacks last (re-seq #"[0-9]+") last parse-long)

        stacks
        (->> stacks
             (butlast)
             (mapv #(nths % (range 1 (* 4 stack-count) 4)))
             (rseq))

        stacks
        (mapv (partial read-stack stacks)
              (range stack-count))

        instructions (->> instructions
                          rest
                          (map parse-instruction))]
    (apply str
           (map last
                (reduce (fn [stacks {:keys [count from to] :as instruction}]
                          (move-fn stacks count from to))
                        stacks
                        instructions)))))

(map simulate [move-9000 move-9001])

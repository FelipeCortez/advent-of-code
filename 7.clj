(ns the-sum-of-its-parts
  (:require [clojure.set :refer [difference union]]))

(def pairs (->> "7.in"
                slurp
                clojure.string/split-lines
                (map #(rest (re-seq #"[A-Z]" %)))))

(def all-nodes (-> pairs flatten set))

(defn conj-in-sorted [m k v]
  (if-let [nodes (get m k)]
    (assoc m k (conj nodes v))
    (assoc m k (sorted-set v))))

(def reqs (reduce (fn [coll v] (apply conj-in-sorted coll v)) {} (map reverse pairs)))

(def first-available (apply sorted-set (difference all-nodes (set (map second pairs)))))

(defn complete-one [reqs which]
  (reduce-kv (fn [m k v]
               (let [visited (disj v which)]
                 (if (seq visited) (assoc m k visited) m)))
             {}
             reqs))

(defn complete-all [reqs]
  (loop [reqs reqs
         available first-available
         result []]
    (if (seq available)
      (let [next-step (first available)
            new-reqs (complete-one reqs next-step)
            new-available (apply sorted-set
                                 (difference (set (keys reqs))
                                             (set (keys new-reqs))))]
        (recur new-reqs
               (union new-available (disj available next-step))
               (conj result next-step)))
      result)))

(println (clojure.string/join (complete-all reqs)))

;; part 2
(defn letter-to-time [base letter]
  (+ 1 base (- (reduce + (map int letter))
               (int \A))))

(defn available-to-pool [available pool seconds max]
  (take max (concat pool
                    (reduce (fn [coll x]
                              (conj coll [x (+ seconds (letter-to-time 60 x))]))
                            []
                            available))))

(defn complete-all-parallel [reqs workers]
  (loop [seconds 0
         job-pool []
         available first-available
         reqs reqs
         result []]
    (if (or (seq reqs) (seq available))
      (let [new-pool (available-to-pool available job-pool seconds workers)
            min-secs (apply min (map second new-pool))
            new-done (->> (filter #(= min-secs (second %)) new-pool)
                          (map first)
                          (apply sorted-set))
            newer-pool (filter #(< min-secs (second %)) new-pool)
            new-reqs (reduce (fn [coll x] (complete-one coll x))
                             reqs
                             new-done)
            now-available (apply sorted-set (difference (set (keys reqs))
                                                        (set (keys new-reqs))))
            new-available (apply sorted-set
                                 (union now-available
                                        (difference available
                                                    (set (map first new-pool)))))]
        (recur min-secs
               newer-pool
               new-available
               new-reqs
               (concat result new-done)))
      [seconds result])))

(println (complete-all-parallel reqs 5))

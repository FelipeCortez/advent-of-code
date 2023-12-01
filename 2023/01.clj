(ns trebuchet?!
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

;; part 1

(->> (slurp "2023/01.in")
     (str/split-lines)
     (map (comp (juxt first peek)
                #(vec (map parse-long (re-seq #"\d" %)))))
     (reduce (fn [sum [a b]] (+ sum (* 10 a) b)) 0))

;; part 2

(def match->num  (into {} (map (fn [x] [(cl-format nil "~R" x)               (str x)])) (range 1 10)))
(def rmatch->num (into {} (map (fn [x] [(str/reverse (cl-format nil "~R" x)) (str x)])) (range 1 10)))
(def ->disjunction-pattern (fn [coll] (re-pattern (format "(?:%s)" (str/join "|" coll)))))

(defn first+last [line]
  [(parse-long (re-find #"\d" (str/replace-first line               (->disjunction-pattern (keys match->num))  match->num)))
   (parse-long (re-find #"\d" (str/replace-first (str/reverse line) (->disjunction-pattern (keys rmatch->num)) rmatch->num)))])

(->> (slurp "2023/01.in")
     (str/split-lines)
     (map first+last)
     (reduce (fn [sum [a b]] (+ sum (* 10 a) b)) 0))

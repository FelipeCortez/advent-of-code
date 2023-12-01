(ns trebuchet?!
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

;; part 1

(->> (slurp "2023/01.in")
     (str/split-lines)
     (map #(vec (map parse-long (re-seq #"\d" %))))
     (map (juxt first peek))
     (reduce (fn [sum [a b]] (+ sum (* 10 a) b)) 0))

;; part 2

(def esrever #(apply str (reverse %)))
(def match->num (into {} (map (fn [x] [(cl-format nil "~R" x) (str x)])) (range 1 10)))
(def rmatch->num (into {} (map (fn [x] [(esrever (cl-format nil "~R" x)) (str x)])) (range 1 10)))
(def pattern-disj (fn [coll] (re-pattern (format "(?:%s)" (str/join (interpose "|" coll))))))

(defn calc-one-line [line]
  [(parse-long (re-find #"\d" (str/replace-first line           (pattern-disj (keys match->num))  match->num)))
   (parse-long (re-find #"\d" (str/replace-first (esrever line) (pattern-disj (keys rmatch->num)) rmatch->num)))])

(->> (slurp "2023/01.in")
     (str/split-lines)
     (map calc-one-line)
     (reduce (fn [sum [a b]] (+ sum (* 10 a) b)) 0))

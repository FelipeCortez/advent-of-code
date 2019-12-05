(ns secure-container
  (:require [clojure.string :as str]))

(def rng (range 367479 (inc 893698)))

(defn never-decreases? [n] (apply <= (map #(Integer/parseInt (str %)) (str n))))

(defn adjacent-digits? [n] (re-find #"([0-9])\1{1,}" (str n)))

(defn one-group-of-two? [n] (seq (filter #(= 2 (count %)) (map first (re-seq #"([0-9])\1{1,}" (str n))))))

;; part 1
(->> rng
     (filter never-decreases?)
     (filter adjacent-digits?)
     count)

;; part 2
(->> rng
     (filter never-decreases?)
     (filter one-group-of-two?)
     count)

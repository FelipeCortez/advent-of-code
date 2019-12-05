(ns secure-container
  (:require [clojure.string :as str]))

(def password-range (range 367479 (inc 893698)))

(defn never-decreases? [n] (apply <= (map #(Integer/parseInt (str %)) (str n))))

(defn adjacent-digits? [n] (re-find #"([0-9])\1{1,}" (str n)))

(defn group-of-just-two? [n] (->> (str n)
                                  (partition-by identity)
                                  (filter #(= 2 (count %)))
                                  seq))

;; part 1
(->> password-range
     (filter (every-pred never-decreases? adjacent-digits?))
     count)

;; part 2
(->> password-range
     (filter (every-pred never-decreases? group-of-just-two?))
     count)

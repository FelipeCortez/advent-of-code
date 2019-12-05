(ns secure-container
  (:require [clojure.string :as str]))

(def password-range (range 367479 (inc 893698)))

(defn never-decreases? [n] (->> (str n) (map #(Integer/parseInt (str %))) (apply <=)))

(defn adjacent-digits? [n] (->> (str n)
                                (partition-by identity)
                                (filter #(>= (count %) 2))
                                seq))

(defn group-of-just-two? [n] (->> (str n)
                                  (partition-by identity)
                                  (filter #(= (count %) 2))
                                  seq))

;; part 1
(->> password-range
     (filter (every-pred never-decreases? adjacent-digits?))
     count)

;; part 2
(->> password-range
     (filter (every-pred never-decreases? group-of-just-two?))
     count)

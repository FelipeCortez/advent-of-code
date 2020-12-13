(ns handy-haversacks
  (:require [clojure.string :as str]))

(defn keywordize [s] (keyword (str/replace s #"\s" "-")))

(defn parse-bags [s]
  (let [[container-s & contained-s]
        (map second (re-seq #"(\d*\s?\w+\s\w+) bags?" s))

        container (keywordize container-s)

        contained
        (if (str/includes? (first contained-s) "no other")
          {}
          (reduce (fn [m s]
                    (let [[_ qty color] (re-find #"(\d+)\s([a-z ]+)" s)]
                      (assoc m (keywordize color) (Integer/parseInt qty))))
                  {}
                  contained-s))]
    [container contained]))

;; part 1
(defn resolve-bags [bag->bags]
  (reduce-kv
   (fn [m bag bags]
     (assoc m bag (visit bag->bags bag)))
   {}
   bag->bags))

(def visit
  (memoize
   (fn [bag->bags bag]
     (let [bags-set (set (keys (bag bag->bags)))]
       (reduce (fn [coll color]
                 (apply conj coll (visit bag->bags color)))
               bags-set
               bags-set)))))

(->> (slurp "07.in")
            (str/split-lines)
            (reduce (fn [m line] (apply assoc m (parse-bags line))) {})
            resolve-bags
            (filter (fn [[_bag bags]] (:shiny-gold bags)))
            count)



;; part 2
(defn sum [bag bag->bags]
  (reduce (fn [total [color qty]]
            (+ total qty (* qty (sum color bag->bags))))
          0
          (bag bag->bags)))

(->> (slurp "07.in")
     (str/split-lines)
     (reduce (fn [m line] (apply assoc m (parse-bags line))) {})
     (sum :shiny-gold))

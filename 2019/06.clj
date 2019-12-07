(ns universal-orbit-map
  (:require [clojure.string :as str]
            [clojure.set]))

;;; part 1
(def orbit-map (->> (slurp "06.in")
                    (str/split-lines)
                    (map (fn [s] (map keyword (str/split s #"\)"))))
                    (reduce (fn [m [orbited orbits]] (assoc m orbits orbited))
                            {})))

(defn orbits-number [m object]
  (loop [object object,number 0]
    (if (object m)
      (recur (object m) (inc number))
      number)))

(let [objects (set (keys orbit-map))]
  (apply + (map (partial orbits-number orbit-map) objects)))

;;; part 2
(def orbit-graph (->> (slurp "06.in")
                      (str/split-lines)
                      (map (fn [s] (map keyword (str/split s #"\)"))))
                      (reduce (fn [m [orbited orbits]] (-> m
                                                           (update orbits #(conj % orbited))
                                                           (update orbited #(conj % orbits))))
                              {})))

(defn shortest [orbit-graph current visited to path]
  (let [visitable (clojure.set/difference (set (current orbit-graph)) visited)]
    (cond
      (= current to)        (do (println (count path) (- (count path) 3) path) path)
      (not (seq visitable)) nil
      :else (reduce (fn [coll object]
                      (if-let [full-path (shortest orbit-graph object (conj visited object) to (conj path object))]
                        (conj coll full-path)
                        coll))
                    []
                    visitable))))

(shortest orbit-graph :YOU #{} :SAN [:YOU])

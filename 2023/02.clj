(ns cube-conundrum
  (:require [clojure.string :as str]))

(def games
  (-> (slurp "2023/02.in")
      (str/split-lines)))

(defn parse-colors [draw]
  (into {}
        (map (fn [[_ cube-count color]]
               [(keyword color)
                (parse-long cube-count)]))
        (re-seq #"(\d+) ((?:red|green|blue))" draw)))

(defn parse-game-id+draws [line]
  (let [[game+id & draws] (str/split line #"[:;]")]
    [(parse-long (re-find #"\d+" game+id))
     (map parse-colors draws)]))

;; part 1

(defn possible-game? [draws]
  (every? (fn [{:keys [red green blue] :or {red 0 green 0 blue 0}}]
            (and (<= red 12) (<= green 13) (<= blue 14)))
          draws))

(->> (map parse-game-id+draws games)
     (map (fn [[game-id draws]] (if (possible-game? draws) game-id 0)))
     (reduce + 0))


;; part 2

(defn game-power [draws]
  (apply * (map (fn [color] (apply max (keep color draws)))
                [:red :green :blue])))

(->> (map parse-game-id+draws games)
     (map (fn [[_game-id draws]] (game-power draws)))
     (reduce +))

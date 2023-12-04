(ns scratchcards
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]))

(defn count-winning [line]
  (let [[_card-number & numbers]
        (re-seq #"(?:\d+|\|)" line)

        [guessed-numbers _ drawn-numbers]
        (partition-by (partial = "|") numbers)]
    (count (set/intersection (set guessed-numbers)
                             (set drawn-numbers)))))

(defn score [winning-count]
  (if (zero? winning-count)
    0
    (int (math/pow 2 (dec winning-count)))))

;; part 1

(->> (slurp "2023/04.in")
     (str/split-lines)
     (map (comp score count-winning))
     (reduce +))

;; part 2

(defn beget-cards [idx winning count cards]
  (reduce (fn [cards idx] (update-in cards [idx :count] + count))
          cards
          (range (inc idx) (inc (+ idx winning)))))

(let [i (volatile! -1)
      cards (->> (slurp "2023/04.in")
                 (str/split-lines)
                 (mapv (fn [line] {:winning (count-winning line), :count 1})))]
  (->> (reduce (fn [cards _]
                 (vswap! i inc)
                 (let [{:keys [winning count]} (get cards @i)]
                   (beget-cards @i winning count cards)))
               cards
               (range (count cards)))
       (map :count)
       (reduce +)))

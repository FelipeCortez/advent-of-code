(ns monkey-in-the-middle
  (:require [clojure.string :as str]))

(defn divisible? [x y] (zero? (rem x y)))

(defn parse-one [lines]
  (let [[_ starting-items updater test truthy falsy]
        (map #(str/replace % #"[a-zA-Z ]+: " "") lines)

        nums #(map parse-long (re-seq #"\d+" %))
        fnums #(first (nums %))]
    {:items (vec (nums starting-items))
     :op (let [operation (get {"+" + "*" *} (re-find #"[+*]" updater))]
           (if-let [operand (fnums updater)]
             (partial operation operand)
             #(operation % %)))
     :test #(divisible? % (fnums test))
     :truthy (fnums truthy)
     :falsy (fnums falsy)
     :inspect-count 0}))

(defn simulate-item [{:keys [op test monkey truthy falsy]} item]
  (let [new-worry (-> item op (quot 3))]
    [(if (test new-worry)
       truthy
       falsy)
     new-worry]))

(defn simulate-item* [{:keys [op test monkey truthy falsy]} item]
  (let [new-worry (op item)

        new-worry (mod new-worry (* 11 2 5 17 19 7 3 13))]
    [(if (test new-worry)
       truthy
       falsy)
     new-worry]))

(defn simulate-turn [monkeys idx]
  (let [{:keys [items] :as monkey} (get monkeys idx)
        monkeys (-> monkeys
                    (update-in [idx :inspect-count] + (count items))
                    (assoc-in [idx :items] []))]
    (->> items
         (map (partial simulate-item* monkey))
         (reduce (fn [monkeys [to-monkey item]]
                   (update-in monkeys [to-monkey :items] conj item))
                 monkeys))))

(defn simulate-round [monkeys]
  (reduce simulate-turn
          monkeys
          (range (count monkeys))))

(->> (slurp "2022/11.in")
     (str/split-lines)
     (partition-all 7)
     (mapv parse-one)
     (iterate simulate-round)
     (take 10001)
     last
     (map :inspect-count)
     (sort >)
     (take 2)
     (reduce *))

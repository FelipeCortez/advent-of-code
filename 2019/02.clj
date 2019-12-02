(ns program-alarm
  (:require [clojure.string :as str]))

(defn split-commas [s] (str/split s #","))

(def input (->> (slurp "02.in")
                (str/trim-newline)
                (split-commas)
                (map #(Integer/parseInt %))
                (into [])))

(defn run-until-halt [noun verb]
  (let [alarm (-> input (assoc 1 noun) (assoc 2 verb))]
    (loop [intcode alarm, pointer 0]
      (let [[opcode op1 op2 store-at] (subvec intcode pointer)]
        (cond
          (= opcode 99)
          intcode

          (= opcode 1)
          (recur (assoc intcode store-at (+ (nth intcode op1) (nth intcode op2)))
                 (+ 4 pointer))

          (= opcode 2)
          (recur (assoc intcode store-at (* (nth intcode op1) (nth intcode op2)))
                 (+ 4 pointer)))))))

;; part 1
(first (run-until-halt 12 2))

;; part 2
(for [noun (range 100)
      verb (range 100)
      :when (= 19690720 (first (run-until-halt noun verb)))]
  (+ (* 100 noun) verb))

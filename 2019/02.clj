(ns program-alarm
  (:require [clojure.string :as str]))

(defn split-commas [s] (str/split s #","))

(def input (->> (slurp "02.in")
                (str/trim-newline)
                (split-commas)
                (map #(Integer/parseInt %))
                (into [])))

(def opcode->fn
  {1  (fn sum [computer [op1 op2 store-at]]
        (-> computer
            (assoc-in [:intcode store-at] (+ (get-in computer [:intcode op1])
                                             (get-in computer [:intcode op2])))
            (update :pointer (partial + 4))))

   2  (fn multiply [computer [op1 op2 store-at]]
        (-> computer
            (assoc-in [:intcode store-at] (* (get-in computer [:intcode op1])
                                             (get-in computer [:intcode op2])))
            (update :pointer (partial + 4))))

   99 (fn halt [computer _] computer)})

(defn run-until-halt [noun verb]
  (loop [computer {:intcode (-> input (assoc 1 noun) (assoc 2 verb)), :pointer 0}]
    (let [{:keys [intcode pointer]} computer
          opcode (nth intcode pointer)
          params (subvec intcode (inc pointer))]
      (if (= 99 opcode)
        computer
        (recur ((opcode->fn opcode) computer params))))))

;; part 1
((comp first :intcode) (run-until-halt 12 2))

;; part 2
(for [noun (range 100)
      verb (range 100)
      :when (= 19690720 ((comp first :intcode) (run-until-halt noun verb)))]
  (+ (* 100 noun) verb))

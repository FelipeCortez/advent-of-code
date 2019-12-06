(ns sunny
  (:require [clojure.string :as str]))

(defn split-commas [s] (str/split s #","))

(def input (->> (slurp "05.in")
                (str/trim-newline)
                (split-commas)
                (map #(Integer/parseInt %))
                (into [])))

(defn opcode-details [opcode+modes]
  (let [op-str (str opcode+modes)
        opcode (Integer/parseInt (apply str (take-last 2 op-str)))
        modes (into [] (reverse (drop-last 2 op-str)))]
    [opcode modes]))

(defn run-until-halt [program]
  (loop [intcode program, pointer 0, output []]
    (let [[opcode+modes & params] (subvec intcode pointer)
          [opcode modes]          (opcode-details opcode+modes)]
      (cond
        (= opcode 99)
        {:intcode intcode
         :output  output}

        (= opcode 1)
        (let [[op1 op2 store-at] params
              [mode1 mode2]      modes]
          (recur (assoc intcode store-at (+ (if (= \1 mode1) op1 (nth intcode op1))
                                            (if (= \1 mode2) op2 (nth intcode op2))))
                 (+ 4 pointer)
                 output))

        (= opcode 2)
        (let [[op1 op2 store-at] params
              [mode1 mode2]      modes]
          (recur (assoc intcode store-at (* (if (= \1 mode1) op1 (nth intcode op1))
                                            (if (= \1 mode2) op2 (nth intcode op2))))
                 (+ 4 pointer)
                 output))

        (= opcode 3)
        (let [[store-at] params]
          (recur (assoc intcode store-at 1)
                 (+ 2 pointer)
                 output))

        (= opcode 4)
        (let [[read-from] params
              [mode]      modes]
          (recur intcode
                 (+ 2 pointer)
                 (conj output (if (= \1 mode) read-from (nth intcode read-from)))))))))


;; part 1
(run-until-halt input)

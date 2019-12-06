(ns sunny
  (:require [clojure.string :as str]))

(defn split-commas [s] (str/split s #","))

(def test-program (->> (slurp "05.in")
                       (str/trim-newline)
                       (split-commas)
                       (map #(Integer/parseInt %))
                       (into [])))

(defn to-int [char] (Integer/parseInt (str char)))

(defn opcode-details [opcode+modes]
  (let [op-str (str opcode+modes)
        opcode (Integer/parseInt (apply str (take-last 2 op-str)))
        modes (into [] (map to-int (reverse (drop-last 2 op-str))))]
    [opcode modes]))

(defn run-until-halt [program input]
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
          (recur (assoc intcode store-at (+ (if (= 1 mode1) op1 (nth intcode op1))
                                            (if (= 1 mode2) op2 (nth intcode op2))))
                 (+ 4 pointer)
                 output))

        (= opcode 2)
        (let [[op1 op2 store-at] params
              [mode1 mode2]      modes]
          (recur (assoc intcode store-at (* (if (= 1 mode1) op1 (nth intcode op1))
                                            (if (= 1 mode2) op2 (nth intcode op2))))
                 (+ 4 pointer)
                 output))

        (= opcode 3)
        (let [[store-at] params]
          (recur (assoc intcode store-at input)
                 (+ 2 pointer)
                 output))

        (= opcode 4)
        (let [[read-from] params
              [mode]      modes
              read-from   (if (= 1 mode) read-from (nth intcode read-from))]
          (recur intcode
                 (+ 2 pointer)
                 (conj output read-from)))

        (= opcode 5)
        (let [[op jump-to]  params
              [mode1 mode2] modes
              op            (if (= 1 mode1) op (nth intcode op))
              jump-to       (if (= 1 mode2) jump-to (nth intcode jump-to))]
          (recur intcode
                 (if (not= 0 op) jump-to (+ 3 pointer))
                 output))

        (= opcode 6)
        (let [[op jump-to]  params
              [mode1 mode2] modes
              op            (if (= 1 mode1) op (nth intcode op))
              jump-to       (if (= 1 mode2) jump-to (nth intcode jump-to))]
          (recur intcode
                 (if (= 0 op) jump-to (+ 3 pointer))
                 output))

        (= opcode 7)
        (let [[op1 op2 store-at] params
              [mode1 mode2]      modes
              op1                (if (= 1 mode1) op1 (nth intcode op1))
              op2                (if (= 1 mode2) op2 (nth intcode op2))
              result             (if (< op1 op2) 1 0)]
          (recur (assoc intcode store-at result)
                 (+ 4 pointer)
                 output))

        (= opcode 8)
        (let [[op1 op2 store-at] params
              [mode1 mode2]      modes
              op1                (if (= 1 mode1) op1 (nth intcode op1))
              op2                (if (= 1 mode2) op2 (nth intcode op2))
              result             (if (= op1 op2) 1 0)]
          (recur (assoc intcode store-at result)
                 (+ 4 pointer)
                 output))))))

;; part 1
(run-until-halt test-program 1)

;; part 2
(run-until-halt test-program 5)

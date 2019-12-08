(ns amplification-circuit
  (:require [clojure.string :as str]))

(defn split-commas [s] (str/split s #","))

(def amp-program (->> (slurp "07.in")
                      (str/trim-newline)
                      (split-commas)
                      (map #(Integer/parseInt %))
                      (into [])))

(defn to-int [char] (Integer/parseInt (str char)))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn split-commas [s] (str/split s #","))

(def test-program (->> (slurp "05.in")
                       (str/trim-newline)
                       (split-commas)
                       (map #(Integer/parseInt %))
                       (into [])))

(defn to-int [char] (Integer/parseInt (str char)))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn opcode-details [opcode+modes]
  (let [op-str (str opcode+modes)
        opcode (Integer/parseInt (apply str (take-last 2 op-str)))
        modes (into [] (map to-int (reverse (drop-last 2 op-str))))]
    [opcode modes]))

(defn resolve-params [{:keys [intcode pointer]}]
  (let [[opcode+modes & params] (subvec intcode pointer)
        [opcode modes]          (opcode-details opcode+modes)]
    (map (fn [param mode] (if-not (= 1 mode) (nth intcode param) param))
         params
         (concat modes (repeat 0)))))

(def opcode->fn
  {1 (fn sum [computer [param1 param2 store-at]]
       (let [[param1* param2*] (resolve-params computer)]
         (-> computer
             (assoc-in [:intcode store-at] (+ param1* param2*))
             (update :pointer (partial + 4)))))

   2 (fn multiply [computer [param1 param2 store-at]]
       (let [[param1* param2*] (resolve-params computer)]
         (-> computer
             (assoc-in [:intcode store-at] (* param1* param2*))
             (update :pointer (partial + 4)))))

   3 (fn store-input [computer [store-at]]
       (-> computer
           (assoc-in [:intcode store-at] (peek (:input computer)))
           (update :input pop)
           (update :pointer (partial + 2))))

   4 (fn output [computer [read-from]]
       (let [[read-from*] (resolve-params computer)]
         (-> computer
             (update :output #(conj % read-from*))
             (update :pointer (partial + 2)))))

   5 (fn jump-if-true [computer [param jump-to]]
       (let [[param* jump-to*] (resolve-params computer)]
         (-> computer
             (update :pointer (if (not= 0 param*) (constantly jump-to*) (partial + 3))))))

   6 (fn jump-if-false [computer [param jump-to]]
       (let [[param* jump-to*] (resolve-params computer)]
         (-> computer
             (update :pointer (if (= 0 param*) (constantly jump-to*) (partial + 3))))))

   7 (fn less-than [computer [param1 param2 store-at]]
       (let [[param1* param2*] (resolve-params computer)]
         (-> computer
             (assoc-in [:intcode store-at] (if (< param1* param2*) 1 0))
             (update :pointer (partial + 4)))))

   8 (fn less-than [computer [param1 param2 store-at]]
       (let [[param1* param2*] (resolve-params computer)]
         (-> computer
             (assoc-in [:intcode store-at] (if (= param1* param2*) 1 0))
             (update :pointer (partial + 4)))))

   99 (fn halt [computer _] computer)})

(defn run-until-halt [program input]
  (loop [computer {:intcode program, :pointer 0, :input (queue input), :output []}]
    (let [{:keys [intcode pointer]} computer
          [opcode+modes & params]   (subvec intcode pointer)
          [opcode modes]            (opcode-details opcode+modes)]
      (if (= 99 opcode)
        computer
        (recur ((opcode->fn opcode) computer params))))))

;;; part 1
(defn compute-amp [last-result phase]
  (-> (run-until-halt amp-program [phase last-result])
      :output
      first))

(defn permutations [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(->> (permutations (range 5))
     (map (fn [phases] (reduce compute-amp 0 phases)))
     (apply max))

;;; part 2
(->> (repeat 5 {:intcode [1 2 3] :pointer 0 :output []})
     (mapv (fn [phase m] (assoc m :input [phase])) [5 6 7 8 9]))

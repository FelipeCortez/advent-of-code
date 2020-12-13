(ns handheld-halting
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[opcode val] (str/split line #"\s")]
    [(keyword opcode) (Integer/parseInt val)]))

(def instructions
  (->> (slurp "08.in")
       (str/split-lines)
       (mapv #(do {:instruction (parse-line %)}))))

(defn simulate [instructions]
  (loop [m {:instructions instructions, :acc 0, :pc 0}]
    (let [{:keys [instructions acc pc]}  m
          {:keys [instruction visited?]} (nth instructions pc)
          [opcode val]                   instruction]
      (if (or visited? (>= pc (dec (count instructions))))
        {:pc pc :acc acc}
        (let [m (assoc-in m [:instructions pc :visited?] true)]
          (recur (case opcode
                   :nop (update m :pc inc)
                   :acc (-> m (update :acc + val) (update :pc inc))
                   :jmp (update m :pc + val))))))))

;; part 1
(simulate instructions)

;; part 2
(apply max-key :pc
       (reduce (fn [coll [idx {:keys [instruction]}]]
                 (let [[opcode _val] instruction]
                   (if (#{:nop :jmp} opcode)
                     (conj coll
                           (simulate (assoc-in instructions
                                               [idx :instruction 0]
                                               ({:nop :jmp, :jmp :nop} opcode))))
                     coll)))
               []
               (map-indexed vector instructions)))

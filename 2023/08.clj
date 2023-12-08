(ns haunted-wasteland
  (:require [clojure.string :as str]))

(defn parse-mapping [line]
  (let [[from l-to r-to] (re-seq #"\w{3}" line)]
    [[[from \L] l-to]
     [[from \R] r-to]]))

(defn parse-input [[instructions _ & mappings]]
  {:instructions (cycle instructions)
   :mappings (into {} (comp (map parse-mapping) cat) mappings)})

(defn step [mappings {:keys [where instructions]}]
  {:where (get mappings [where (first instructions)])
   :instructions (next instructions)})

(let [{:keys [mappings instructions]}
      (->> (slurp "2023/08.in") (str/split-lines) parse-input)]
  (count (take-while #(not= (:where %) "ZZZ")
                     (iterate (partial step mappings) {:where "AAA", :instructions instructions}))))


;; part 2

(defn gcd [a b] (loop [a (abs a) b (abs b)] (if (zero? b) a, (recur b (mod a b)))))
(defn lcm [a b] (cond (zero? a) 0, (zero? b) 0, :else (* b (quot a (gcd a b)))))

(defn az-length [{:keys [mappings instructions]} starting]
  (->> (iterate (partial step mappings) {:where starting, :instructions instructions})
       (take-while #(not (str/ends-with? (:where %) "Z")))
       (count)))

(let [{:keys [mappings instructions] :as mappings+instructions}
      (->> (slurp "2023/08.in") (str/split-lines) (parse-input))

      starting-points
      (filter #(str/ends-with? % "A") (map ffirst mappings))]
  (reduce lcm (map (partial az-length mappings+instructions) starting-points)))

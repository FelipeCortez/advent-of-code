(require '[clojure.string :as str])

(def shl bit-shift-left)
(def shr bit-shift-right)
(def xor bit-xor)
(def first-23-bits #(bit-and % (dec (bit-shift-left 1 24))))

(defn step-1 [x] (first-23-bits (xor x (shl x  6))))
(defn step-2 [x] (first-23-bits (xor x (shr x  5))))
(defn step-3 [x] (first-23-bits (xor x (shl x 11))))

(defn secrete [x] (-> x step-1 step-2 step-3))
(defn secrete-2000th [x] (first (drop 2000 (iterate secrete x))))

(def initial-secrets (map parse-long (str/split-lines (slurp "2024/22.in"))))

(reduce + (pmap secrete-2000th initial-secrets))
;; => 12759339434

(def test-sequence [1 2 3 2024])
(defn assoc-nx [m k v] (if (get m k) m (assoc m k v)))
(defn unit [x] (mod x 10))
(defn differences [xs] (mapv #(- (second %) (first %)) (partition 2 1 (map unit xs))))

(defn seq->price [x]
  (reduce (fn [m [k v]] (assoc-nx m k v))
          {}
          (->> (iterate secrete x)
               (take 2000)
               (partition 5 1)
               (pmap (juxt differences (comp unit last))))))

(apply max
       (vals (reduce (partial merge-with +)
                     (pmap seq->price initial-secrets))))

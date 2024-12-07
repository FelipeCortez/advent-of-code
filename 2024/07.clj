(require '[clojure.string :as str])

(def grow (memoize (fn [oplists] (mapcat (fn [oplist] [(conj oplist *) (conj oplist +)]) oplists))))
(defn ops [len] (nth (iterate grow [[+] [*]]) (- len 2)))

(defn sim [goal [fst & rest] oplist]
  (= goal
     (reduce (fn [acc [number op]]
               (if (> acc goal) (reduced nil) (op acc number)))
             fst
             (map vector rest oplist))))

(->> (slurp "2024/07.in")
     (str/split-lines)
     (keep (fn [line] (let [[goal & numbers] (map parse-long (re-seq #"\d+" line))]
                        (when (some (partial sim goal numbers)
                                    (ops (count numbers)))
                          goal))))
     (reduce +))

(defn numcat [a b] (long (+ b (* a (Math/pow 10 (inc (Math/floor (Math/log10 b))))))))
(defn numcat' [a b] (parse-long (str a b)))

(time (numcat 9000 1))
(time (numcat' 9000 1))

(def grow' (memoize (fn [oplists] (mapcat (fn [oplist] [(conj oplist *) (conj oplist +) (conj oplist numcat')]) oplists))))
(defn ops' [len] (nth (iterate grow' [[+] [*] [numcat]]) (- len 2)))


(->> (slurp "2024/07.in")
     (str/split-lines)
     (keep (fn [line] (let [[goal & numbers] (map parse-long (re-seq #"\d+" line))]
                        (when (some (partial sim goal numbers)
                                    (ops' (count numbers)))
                          goal))))
     (reduce +))

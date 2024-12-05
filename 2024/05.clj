(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn good-order? [rules page]
  (let [pages-set (set page)
        page->idx (into {} (map-indexed (fn [idx page] [page idx])) page)]
    (every? (fn [[pred succ]]
              (or (not (set/subset? #{pred succ} pages-set))
                  (< (page->idx pred) (page->idx succ))))
            rules)))

(defn page-rules [rules page]
  (let [pages-set (set page)]
    (filter (fn [[pred succ]] (set/subset? #{pred succ} pages-set))
            rules)))

(defn rules->sort-keyfn [rules]
  (loop [rules rules
         ordering '()
         remaining (into #{} cat rules)]
    (if (seq remaining)
      (let [no-succs (set/difference remaining (into #{} (map first rules)))
            no-succ (first no-succs)]
        (assert (= 1 (count no-succs)))
        (recur (remove (fn [[_pred succ]] (= no-succ succ)) rules)
               (conj ordering no-succ)
               (disj remaining no-succ)))
      (into {} (map-indexed (fn [idx page] [page idx]))
            ordering))))

(let [[rules [_ & p-updates]]
      (->> (slurp "2024/05.in")
           (str/split-lines)
           (split-with (complement str/blank?)))

      rules     (map #(str/split % #"\|") rules)
      p-updates (map #(str/split % #",") p-updates)]
  [(->> p-updates
        (filter (partial good-order? rules))
        (map (fn [xs] (parse-long (nth xs (/ (count xs) 2)))))
        (reduce +))
   (->> p-updates
        (remove (partial good-order? rules))
        (map (fn [xs]
               (let [keyfn (rules->sort-keyfn (page-rules rules xs))]
                 (sort-by keyfn xs))))
        (map (fn [xs] (parse-long (nth xs (/ (count xs) 2)))))
        (reduce +))])

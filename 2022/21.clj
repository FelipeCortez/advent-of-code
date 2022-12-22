(ns monkey-math
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[monkey dep1 dep2 op] (concat (re-seq #"[a-z]{4}" line)
                                      (re-seq #"[+-/\*]" line))]
    (if-let [n (re-find #"[0-9]+" line)]
      {monkey (parse-long n)}
      {monkey [(eval (read-string op)) dep1 dep2]})))


(defn simulate [initial]
  (let [monkeys->ops
        (->> (slurp "2022/21.in")
             (str/split-lines)
             (map parse-line)
             (into {}))

        monkeys->ops
        (if initial
          (assoc monkeys->ops "humn" initial)
          monkeys->ops)

        promises (zipmap (keys monkeys->ops)
                         (repeatedly promise))]
    (doseq [[monkey n]
            (filter (comp number? val) monkeys->ops)]
      (deliver (get promises monkey) n))

    (doseq [[monkey [op dep1 dep2]]
            (filter (comp not number? val) monkeys->ops)]
      (future
        (deliver (get promises monkey)
                 (op @(get promises dep1)
                     @(get promises dep2)))))
    [@(get promises "root")

     (- @(get promises "fglq")
        @(get promises "fzbp"))]))

;; part 1
(first (simulate nil))

;; part 2
(map (juxt identity (comp second simulate))
     (take 100 (iterate (partial + 1) 3.5876475628E12))) ;; (zoomed in manually)

(ns no-space-left-on-device
  (:require [clojure.string :as str]))

(loop [cmds (str/split-lines (slurp "07.in"))
       m {:path [] :sizes {}}]
  (let [[line & lines] cmds]
    (cond
      (not (seq line))
      [(->> (:sizes m)
            vals
            (filter #(<= % 100000))
            (reduce +))
       (let [unused (- 70000000 (get-in m [:sizes ["/"]]))
             needed (- 30000000 unused)]
         (->> (:sizes m)
              (filter #(>= (val %) needed))
              (apply min-key val)))]

      (= line "$ cd ..")
      (recur lines (update m :path pop))

      (str/starts-with? line "$ cd")
      (recur lines
             (update m :path conj (subs line 5)))

      (str/starts-with? line "$ ls")
      (let [[in-path other-cmds]
            (split-with #(not (str/starts-with? % "$"))
                        lines)

            du (->> (filter #(not (str/starts-with? % "dir")) in-path)
                    (map (comp parse-long (partial re-find #"[0-9]+")))
                    (reduce +))]
        (recur other-cmds
               (assoc m :sizes
                      (reduce (fn [sizes path]
                                (update sizes path (fnil + 0) du))
                              (:sizes m)
                              (rest (reductions conj [] (:path m)))))))

      :else m)))

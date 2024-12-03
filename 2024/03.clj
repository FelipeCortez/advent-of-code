(require '[clojure.string :as str])

(->> (slurp "2024/03.in")
     (re-seq #"mul\((\d+),(\d+)\)")
     (transduce (map (fn [[_match op1-s op2-s]]
                       (* (parse-long op1-s)
                          (parse-long op2-s))))
                + 0))

(->> (slurp "2024/03.in")
     (re-seq #"don't\(\)|do\(\)|mul\((\d+),(\d+)\)")
     (reduce (fn [[total enabled?] [instruction op1-s op2-s]]
               (condp #(str/starts-with? %2 %1) instruction
                 "mul"   [(if enabled?
                            (+ total (* (parse-long op1-s)
                                        (parse-long op2-s)))
                            total)
                          enabled?]
                 "do()"    [total true]
                 "don't()" [total false]))
             [0 true]))

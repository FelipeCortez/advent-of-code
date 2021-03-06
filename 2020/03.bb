#!/usr/bin/env bb

(require '[babashka.process :refer [process]])

(def read-int #(Integer/parseInt (first (str/split-lines %))))

(->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
     (map (fn [[right down]]
              (-> (process ['./a.out right down]
                           {:in (slurp "03.in")})
                  :out
                  slurp
                  read-int)))
     (apply *))

(ns wait-for-it)

(defn ways-to-win [[duration record]]
  (count (for [i (range (inc duration))
               :let [travel-time (- duration i)]
               :when (> (* travel-time i) record)]
           i)))

(reduce * (map ways-to-win durations+records))

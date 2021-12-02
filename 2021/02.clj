(ns dive!)

(def example
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(def cmds
  (map (fn [s] (update (clojure.string/split s #" ") 1 read-string))
       (clojure.string/split-lines (slurp "02.in"))))

;; part 1
(->> cmds
     (reduce
      (fn [acc cmd]
        (let [[instruction val] cmd]
          (case instruction
            "forward" (update acc :h #(+ val %))
            "up"      (update acc :depth #(- % val))
            "down"    (update acc :depth #(+ val %)))))
      {:h 0 :depth 0})
     vals
     (apply *))

;; part 2
(as-> cmds x
      (reduce
       (fn [acc cmd]
         (let [[instruction val] cmd]
           (case instruction
             "forward" (let [{:keys [h depth aim]} acc]
                         (-> acc
                             (update :h #(+ val %))
                             (update :depth #(+ (* val aim) %))))
             "up"      (update acc :aim #(- % val))
             "down"    (update acc :aim #(+ val %)))))
       {:h 0 :depth 0 :aim 0}
       x)
      (dissoc x :aim)
      (vals x)
      (apply * x))

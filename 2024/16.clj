(require '[clojure.string :as str])
(import '[java.util PriorityQueue])

(def maze (str/split-lines (slurp "2024/16.in")))

(let [*walls (transient #{})]
  (doseq [i (range (count maze)), j (range (count maze))]
    (case (get-in maze [i j])
      \# (conj! *walls [i j])
      \S (def start [i j])
      \E (def goal [i j])
      nil))
  (def walls (persistent! *walls)))

(defn p+ [p1 p2] (mapv + p1 p2))

(def dir->delta {:e [0 1] :w [0 -1] :n [-1 0] :s [1 0]})
(def cw  {:n :e :e :s :s :w :w :n})
(def ccw {:n :w :w :s :s :e :e :n})

(defn neighbors [[position direction]]
  (cond-> [[position (cw direction)]
           [position (ccw direction)]]
    (not (walls (p+ position (dir->delta direction))))
    (conj [(p+ position (dir->delta direction)) direction])))

(defn cost-of-move [[_ d1] [_ d2]] (if (= d1 d2) 1 1000))

(def dijkstra
  (loop [frontier (doto (PriorityQueue.) (.add [0 [start :e]]))
         came-from {}
         cost-so-far {[start :e] 0}]
    (if (empty? frontier)
      [came-from cost-so-far]
      (let [[distance current] (.poll frontier)
            [frontier came-from cost-so-far]
            (reduce (fn [[frontier came-from cost-so-far] neighbor]
                      (let [cost (+ (cost-of-move current neighbor)
                                    (get cost-so-far current))]
                        (cond
                          (< cost (get cost-so-far neighbor ##Inf))
                          [(doto frontier (.add [cost neighbor]))
                           (assoc came-from neighbor [current])
                           (assoc cost-so-far neighbor cost)]

                          (= cost (get cost-so-far neighbor ##Inf))
                          [(doto frontier (.add [cost neighbor]))
                           (update came-from neighbor conj current)
                           (assoc cost-so-far neighbor cost)]

                          (> cost (get cost-so-far neighbor ##Inf))
                          [frontier came-from cost-so-far])))
                    [frontier came-from cost-so-far]
                    (neighbors current))]
        (.remove frontier [distance current])
        (recur frontier came-from cost-so-far)))))

(def came-from (first dijkstra))
(def costs (second dijkstra))

(def min-cost
  (min (get costs [goal :s])
       (get costs [goal :n])
       (get costs [goal :e])
       (get costs [goal :w])))

(def goal+dirs
  (into []
        (keep (fn [dir] (when (= (get costs [goal dir]) min-cost) [goal dir])))
        [:s :n :e :w]))

(loop [to-visit goal+dirs, visited #{}]
  (if (empty? to-visit)
    (count (distinct (map first visited)))
    (let [visiting (peek to-visit)]
      (recur (into (pop to-visit) (came-from visiting))
             (conj visited visiting)))))

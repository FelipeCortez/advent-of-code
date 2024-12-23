(require '[clojure.string :as str]
         '[clojure.set :as set])

(def conns
  (map (partial re-seq #"[a-z]{2}")
       (str/split-lines (slurp "2024/23.in"))))

(def com->coms
  (reduce (fn [com->coms [com1 com2]]
            (-> com->coms
                (update com1 (fnil conj #{}) com2)
                (update com2 (fnil conj #{}) com1)))
          {}
          conns))

(def triples
  (reduce (fn [triples com1]
            (let [com1-coms (get com->coms com1)]
              (reduce (fn [triples com2]
                        (let [com2-coms (get com->coms com2)]
                          (if-let [com3s (seq (set/intersection com1-coms com2-coms))]
                            (into triples (map (fn [com3] #{com1 com2 com3}) com3s))
                            triples)))
                      triples
                      com1-coms)))
   #{}
   (keys com->coms)))

(count (filter (fn [triple] (some #(str/starts-with? % "t") triple)) triples))
;; => 893

(defn max-clique' [*cliques r p x]
  (when (and (empty? p) (empty? x))
    (conj! *cliques (sort r)))
  (loop [vs p, p p, x x]
    (when-not (empty? vs)
      (let [v (first vs), neighbors (com->coms v)]
        (max-clique' *cliques
                     (conj r v)
                     (set/intersection p neighbors)
                     (set/intersection x neighbors))
        (recur (disj vs v) (disj p v) (conj x v))))))

(defn max-clique []
  (let [*cliques (transient [])]
    (max-clique' *cliques #{} (set (keys com->coms)) #{})
    (persistent! *cliques)))

(str/join "," (apply max-key count (max-clique)))
;; => "cw,dy,ef,iw,ji,jv,ka,ob,qv,ry,ua,wt,xz"

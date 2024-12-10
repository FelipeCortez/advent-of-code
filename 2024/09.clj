(require '[clojure.string :as str])

(def areas (str/trim (slurp "2024/09.in")))
(defn char->num [c] (- (int c) (int \0)))

(def files+spaces
  (loop [areas areas
         types (cycle [:file :space])
         file-number 0
         idx 0
         output {:files [], :spaces []}]
    (if areas
      (let [[area] areas, [area-type] types]
        (recur (next areas)
               (next types)
               (if (= :file area-type) (inc file-number) file-number)
               (+ idx (char->num area))
               (if (= :file area-type)
                 (update output :files  conj [file-number idx (char->num area)])
                 (update output :spaces conj [idx (char->num area)]))))
      output)))

(defn checksum [files]
  (reduce + (map (fn [[number idx area]]
                   (* number (/ area 2) (+ (* idx 2)
                                           (dec area))))
                 files)))

(loop [files+spaces files+spaces, new-files []]
  (let [[f-number f-idx f-area :as _file] (peek (:files files+spaces))
        [s-idx s-area :as space]  (first (:spaces files+spaces))]
    (if (and (seq space) (> f-idx s-idx))
      (cond
        (> s-area f-area)
        (recur (-> files+spaces
                   (update :files pop)
                   (assoc-in [:spaces 0] [(+ s-idx f-area) (- s-area f-area)]))
               (conj new-files [f-number s-idx f-area]))

        (= s-area f-area)
        (recur (-> files+spaces
                   (update :files pop)
                   (update :spaces subvec 1))
               (conj new-files [f-number s-idx s-area]))

        (< s-area f-area)
        (recur (-> files+spaces
                   (update :files pop)
                   (update :files conj [f-number f-idx (- f-area s-area)])
                   (update :spaces subvec 1))
               (conj new-files [f-number s-idx s-area])))
      (checksum (into (:files files+spaces) new-files)))))

(defn leftmost-that-fits [spaces [f-idx f-area]]
  (when f-area
    (some->> spaces
             (take-while (fn [[s-idx _s-area]] (<= s-idx f-idx)))
             (filter (fn [[_s-idx s-area]] (<= f-area s-area)))
             (seq)
             (apply min-key first))))

(let [files+spaces' (update files+spaces :spaces (partial into (sorted-map)))]
  (loop [files+spaces files+spaces', new-files []]
    (let [[f-number f-idx f-area :as file] (peek (:files files+spaces))
          [s-idx s-area :as space] (leftmost-that-fits (:spaces files+spaces) [f-idx f-area])]
      (if (seq (:files files+spaces))
        (cond
          (not space)
          (recur (update files+spaces :files pop)
                 (conj new-files file))

          (> s-area f-area)
          (recur (-> files+spaces
                     (update :files pop)
                     (update :spaces dissoc s-idx)
                     (assoc-in [:spaces (+ s-idx f-area)] (- s-area f-area)))
                 (conj new-files [f-number s-idx f-area]))

          (= s-area f-area)
          (recur (-> files+spaces
                     (update :files pop)
                     (update :spaces dissoc s-idx))
                 (conj new-files [f-number s-idx s-area])))
        (checksum new-files)))))

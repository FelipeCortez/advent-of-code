(ns care-package
  (:require [clojure.string :as str]))


(defn split-commas [s] (str/split s #","))

(defn to-int [char] (Integer/parseInt (str char)))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn opcode-details [opcode+modes]
  (let [op-str (str opcode+modes)
        opcode (Integer/parseInt (apply str (take-last 2 op-str)))
        modes (into [] (map to-int (reverse (drop-last 2 op-str))))]
    [opcode modes]))

(defn vec->map [intcode]
  (into (sorted-map) (map-indexed (fn [idx v] [idx v]) intcode)))

(defn submap [m idx]
  (lazy-seq (cons (get m idx 0) (submap m (inc idx)))))

(defn resolve-params
  ([computer] (resolve-params computer nil))
  ([{:keys [intcode pointer relative-base]} store-at]
   (let [[opcode+modes & params] (submap intcode pointer)
         [opcode modes]          (opcode-details opcode+modes)]
     (map (fn [idx param mode]
            (if (= idx store-at)
              (cond
                (= 0 mode) param
                (= 2 mode) (+ param relative-base)
                :else      (ex-info "should never happen"))
              (cond
                (= 0 mode) (get intcode param 0)
                (= 1 mode) param
                (= 2 mode) (get intcode (+ param relative-base) 0))))
          (range)
          params
          (concat modes (repeat 0))))))

(def opcode->fn
  {1 (fn sum [computer]
       (let [[param1 param2 store-at] (resolve-params computer 2)]
         (-> computer
             (assoc-in [:intcode store-at] (+ param1 param2))
             (update :pointer (partial + 4)))))

   2 (fn multiply [computer]
       (let [[param1 param2 store-at] (resolve-params computer 2)]
         (-> computer
             (assoc-in [:intcode store-at] (* param1 param2))
             (update :pointer (partial + 4)))))

   3 (fn store-input [computer]
       (let [[store-at] (resolve-params computer 0)]
         (-> computer
             (assoc-in [:intcode store-at] (peek (:input computer)))
             (update :input pop)
             (update :pointer (partial + 2)))))

   4 (fn output [computer]
       (let [[read-from] (resolve-params computer)]
         (-> computer
             (update :output #(conj % read-from))
             (update :pointer (partial + 2)))))

   5 (fn jump-if-true [computer]
       (let [[param jump-to] (resolve-params computer)]
         (-> computer
             (update :pointer (if (not= 0 param) (constantly jump-to) (partial + 3))))))

   6 (fn jump-if-false [computer]
       (let [[param jump-to] (resolve-params computer)]
         (-> computer
             (update :pointer (if (= 0 param) (constantly jump-to) (partial + 3))))))

   7 (fn less-than [computer]
       (let [[param1 param2 store-at] (resolve-params computer 2)]
         (-> computer
             (assoc-in [:intcode store-at] (if (< param1 param2) 1 0))
             (update :pointer (partial + 4)))))

   8 (fn equal-to [computer]
       (let [[param1 param2 store-at] (resolve-params computer 2)]
         (-> computer
             (assoc-in [:intcode store-at] (if (= param1 param2) 1 0))
             (update :pointer (partial + 4)))))

   9 (fn adjust-relative-base [computer]
       (let [[param] (resolve-params computer)]
         (-> computer
             (update :relative-base (partial + param))
             (update :pointer (partial + 2)))))

   99 (fn halt [computer _] computer)})

(def arcade-program (->> (slurp "13.in")
                         (str/trim-newline)
                         (split-commas)
                         (map read-string)
                         (into [])))

(defn setup-until-halt [program]
  (loop [computer {:intcode       (vec->map program)
                   :pointer       0
                   :input         (queue)
                   :output        '()
                   :relative-base 0}
         game     {:position {:x 0 :y 0}
                   :action   (cycle [:set-x :set-y :set-tile])
                   :world    {}}]
    (let [{:keys [intcode pointer relative-base output]} computer
          [opcode+modes & params]                        (submap intcode pointer)
          [opcode modes]                                 (opcode-details opcode+modes)]
      (cond
        (= 99 opcode)
        {:computer computer, :world (:world game)}

        (= 4 opcode)
        (let [computer ((opcode->fn opcode) computer)
              game (case (first (get game :action))
                     :set-x    (assoc-in game [:position :x] (first (:output computer)))
                     :set-y    (assoc-in game [:position :y] (first (:output computer)))
                     :set-tile (assoc-in game [:world (:position game)] (first (:output computer))))]
          (recur (update computer :output rest)
                 (update game :action rest)))

        :else
        (recur ((opcode->fn opcode) computer) game)))))

;;; part 1
(->> (setup-until-halt arcade-program)
     :world
     (filter (fn [[k v]] (= 2 v)))
     count)

;;; part 2
(defn draw-game
  [world]
  (let [max-x (->> world (map (fn [[{x :x} _]] x)) (apply max))
        max-y (->> world (map (fn [[{y :y} _]] y)) (apply max))]
    (->> (for [y (range 0 (inc max-y)) x (range 0 (inc max-x))] (get world {:x x :y y}))
         (partition (inc max-x))
         (map str/join)
         (map (fn [s] (str/replace s #"0" " ")))
         (map (fn [s] (str/replace s #"1" "X")))
         (map (fn [s] (str/replace s #"2" "·")))
         (map (fn [s] (str/replace s #"3" "_")))
         (map (fn [s] (str/replace s #"4" "o")))
         (map println)
         doall)))

(defn play-until-halt [program]
  (loop [computer {:intcode       (vec->map program)
                   :pointer       0
                   :input         (queue)
                   :output        '()
                   :relative-base 0}
         game     {:position {:x 0 :y 0}
                   :action   (cycle [:set-x :set-y :set-tile])
                   :world    {}}]
    (let [{:keys [intcode pointer relative-base output]} computer
          [opcode+modes & params]                        (submap intcode pointer)
          [opcode modes]                                 (opcode-details opcode+modes)]
      (cond
        (= 99 opcode)
        {:computer computer, :world (:world game)}

        (= 3 opcode)
        (do (draw-game (:world game))
            (recur (-> computer
                       (update :input #(conj % (read-string (read-line))))
                       ((opcode->fn opcode)))
                   game))

        (= 4 opcode)
        (let [computer ((opcode->fn opcode) computer)
              game (case (first (get game :action))
                     :set-x    (assoc-in game [:position :x] (first (:output computer)))
                     :set-y    (assoc-in game [:position :y] (first (:output computer)))
                     :set-tile (assoc-in game [:world (:position game)] (first (:output computer))))]
          (recur (update computer :output rest)
                 (update game :action rest)))

        :else
        (recur ((opcode->fn opcode) computer) game)))))

(play-until-halt (assoc arcade-program 0 2))

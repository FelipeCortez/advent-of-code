(ns pipe-maze
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-and-find-s [lines]
  (let [s-pos* (atom nil)
        grid (into []
                   (map-indexed
                    (fn [j line]
                      (into []
                            (map-indexed (fn [i pipe]
                                           (when (= \S pipe) (reset! s-pos* [j i]))
                                           pipe))
                            line)))
                   lines)]
    {:grid grid
     :starting-position @s-pos*}))

(let [{:keys [grid starting-position]}
      (->> (slurp "2023/10.in") (str/split-lines) parse-and-find-s)]
  (def grid grid)
  (def starting-position starting-position))

(def down [1 0]) (def up [-1 0]) (def left [0 -1]) (def right [0 1])
(def pipe+dir->dir
  {[\| up    ] up
   [\| down  ] down
   [\- right ] right
   [\- left  ] left
   [\L down  ] right
   [\L left  ] up
   [\J down  ] left
   [\J right ] up
   [\F left  ] down
   [\F up    ] right
   [\7 right ] down
   [\7 up    ] left})

(defn step [{:keys [pos dir distances current-distance]}]
  (let [this-pipe (get-in grid pos)
        new-dir (get pipe+dir->dir [this-pipe dir])
        new-pos (mapv + pos new-dir)]
    {:pos new-pos
     :dir new-dir
     :distances (assoc distances new-pos (inc current-distance))
     :current-distance (inc current-distance)
     :this-pipe this-pipe}))

(let [path1
      (take-while #(not= (:pos %) starting-position)
                  (iterate step
                           {:distances {starting-position 0}
                            :current-distance 1
                            :dir [0 1]
                            :pos (mapv + starting-position [0 1])}))

      path2
      (take-while #(not= (:pos %) starting-position)
                  (iterate step
                           {:distances {starting-position 0}
                            :current-distance 1
                            :dir [0 -1]
                            :pos (mapv + starting-position [0 -1])}))]
  (sort-by (comp - val)
           (merge-with min
                       (:distances (last path1))
                       (:distances (last path2)))))

;; part 2

(def path
  (set
   (map key
        (:distances
         (last
          (take-while #(not= (:pos %) starting-position)
                      (iterate step
                               {:distances {starting-position 0
                                            (mapv + starting-position [0 1]) 1}
                                :current-distance 1
                                :dir [0 1]
                                :pos (mapv + starting-position [0 1])})))))))

(def clean-grid
  (into []
        (map-indexed
         (fn [j line]
           (into []
                 (map-indexed (fn [i pipe]
                                (cond
                                  (= \S pipe) -
                                  (contains? path [j i]) pipe
                                  :else \.)))
                 line)))
        grid))


(def expand
  {\| [".X."
       ".X."
       ".X."]
   \- ["..."
       "XXX"
       "..."]
   \L [".X."
       ".XX"
       "..."]
   \7 ["..."
       "XX."
       ".X."]
   \J [".X."
       "XX."
       "..."]
   \F ["..."
       ".XX"
       ".X."]
   \. ["..."
       "..."
       "..."]})

(defn parse-and-expand [lines]
  (into []
        (mapcat (fn [line]
                  (reduce (fn [new-lines thing] (map into new-lines (expand thing)))
                          [[] [] []]
                          line)))
        lines))

(defn psum [a b] (mapv + a b))

(defn neighbors [grid pos]
  (into #{}
        (comp (map (partial psum pos))
              (filter #(= \. (get-in grid %))))
        [[0 1] [0 -1] [-1 0] [1 0]]))

(defn flood-fill [grid]
  (loop [visited #{}, queue [[0 0]]]
    (if (seq queue)
      (recur (conj visited (peek queue))
             (into (pop queue) (set/difference (neighbors grid (peek queue))
                                               visited)))
      [grid visited])))

(defn visualize [[grid visited]]
  (reduce (fn [grid pos]
            (assoc-in grid pos \O))
          grid
          visited))

(defn contract [grid]
  (count
   (for [j (mapv #(inc (* 3 %)) (range (/ (count grid) 3)))
         i (mapv #(inc (* 3 %)) (range (/ (count (first grid)) 3)))
         :when (every? #(= \. %)
                       (mapv (comp (partial get-in grid) (partial psum [j i]))
                             [[-1 -1] [-1 0] [-1 1]
                              [ 0 -1] [ 0 0] [ 0 1]
                              [ 1 -1] [ 1 0] [ 1 1]]))]
     1)))

(defn see [grid] (run! println grid))

(->> clean-grid
     parse-and-expand
     flood-fill
     visualize
     contract)

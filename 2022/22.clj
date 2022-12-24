(ns monkey-map
  (:require [clojure.string :as str]
            [clojure.set :as set]))

@(def m+instructions
  (let [in (str/split-lines (slurp "2022/22.in"))
        m (subvec in 0 (- (count in) 2))

        moves
        (->> (re-seq #"[0-9]+" (last in))
             (map parse-long))

        rotations
        (->> (re-seq #"[RL]" (last in))
             (map first))]
    [m (interleave moves rotations)]))

(def m (first m+instructions))

(defn starting-position [m]
  [0 (->> m
          first
          (keep-indexed (fn [idx val] (when (not= \space val) idx)))
          first)])

(def starting-direction \E)

(def dir->delta
  {\N [-1  0]
   \S [ 1  0]
   \E [ 0  1]
   \W [ 0 -1]})

(def cw
  {\N \E
   \E \S
   \S \W
   \W \N})

(def ccw (set/map-invert cw))

(defn m+ [pos1 pos2] (mapv + pos1 pos2))

(starting-position (first m+instructions))

(defn wrap [m pos dir]
  (let [js (count m)
        is (apply max (map count m))]
    (loop [pos pos]
      (let [[j i] pos
            pos [(mod j js) (mod i is)]]
        (if (= \space (get-in m pos \space))
          (recur (m+ pos (dir->delta dir)))
          pos)))))

(declare redraw!)

(defn simulate [[m instructions]]
  (loop [position (starting-position m)
         direction starting-direction
         instructions instructions
         wraps []]
    (let [[instruction & instructions] instructions]
      (cond
        (number? instruction)
        (let [new-position (reduce (fn [position _]
                                     (let [new-position (wrap m
                                                              (m+ position (dir->delta direction))
                                                              direction)]
                                       (case (get-in m new-position)
                                         \# position
                                         \. new-position)))
                                   position
                                   (range instruction))]
          (recur
           new-position
           direction
           instructions
           (if (some #(> % 1)
                     (map #(Math/abs (- %1 %2))
                          new-position
                          position))
             (conj wraps position new-position))))

        (char? instruction)
        (recur position
               (case instruction
                 \R (cw direction)
                 \L (ccw direction))
               instructions
               wraps)


        :else
        (let [[j i] position]
          [(+ (* 1000 (inc j))
              (* 4 (inc i))
              ({\E 0, \S 1, \W 2, \N 3} direction))
           wraps])))))

(defn color-grid [_dir pos]
  (let [[m _] m+instructions
        js (count m)
        is (apply max (map count m))]
    (mapv vec
          (partition-all
           is
           (for [j (range js)
                 i (range is)]
             (if (= pos [j i])
               0x000000FF
               (case (get-in m [j i] \space)
                 \space 0xFFFFFF00
                 \.     0xFFFF00FF
                 \#     0xFF00FFFF)))))))

(def !pos (atom [35 111]))
(def !dir (atom \N))

(defn move! []
  (let [new-position (wrap m (m+ @!pos (dir->delta @!dir))
                           @!dir)]
    (when (= \. (get-in m new-position))
      (reset! !pos new-position))))

(comment
  (do
    (move!)
    (redraw! @!pos))
  (swap! !dir cw)
  (swap! !dir ccw)
  )

(require '[io.github.humbleui.ui :as ui]
         '[io.github.humbleui.paint :as paint])

(defn redraw! [pos]
  (def ui
    (ui/key-listener
     {:on-key-down #(case (:key %)
                      :left (swap! !dir ccw)
                      :right (swap! !dir cw)
                      :up   (move!))}
     (ui/with-bounds ::bounds
         (ui/dynamic
          ctx [height (:height (::bounds ctx))
               width (:width (::bounds ctx))
               pos @!pos]
          (println @!pos)
          (let [grid (color-grid nil pos)
                [m _] m+instructions]
            (ui/default-theme
             {}
             (ui/rect
              (paint/fill 0xFF000000)
              (ui/padding
               10 10
               (ui/row
                (interpose
                 (ui/gap 0 1)
                 (for [i (range (count (first grid)))]
                   [:stretch 1
                    (ui/column
                     (interpose
                      (ui/gap 1 0)
                      (for [j (range (count grid))]
                        [:stretch 1
                         (ui/rect (paint/fill
                                   (if (#{#_#_[33 111] [35 111]} [j i])
                                     0xFFFFFFFF
                                     (get-in grid [j i])))
                                  (ui/padding 5 10
                                              (ui/label "" #_(str i "_" j))))])))]))))))))))))

(comment
  (def animate (future (simulate m+instructions)))
  (future-cancel animate)
  (redraw! [0 0])

  (ui/start-app!
   (ui/window
    {:title "Viz"}
    #'ui))
  nil)

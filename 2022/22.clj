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

(defn rangei [from to]
  (if (< from to)
    (range from (inc to))
    (range from (dec to) -1)))

(defn range-2d [[i1 j1] [i2 j2]]
  (if (= i1 i2)
    (mapv vector (repeat i1)    (rangei j1 j2))
    (mapv vector (rangei i1 i2) (repeat j1))))

;; reduces from [i j] to [j i] because I'm stupid
(def portals
  (reduce (fn [s [[from-dir from1 to1] [to-dir from2 to2]]]
            (merge s
                   (into {}
                         (mapv (fn [[i1 j1] [i2 j2]]
                                 [[from-dir [j1 i1]]
                                  [to-dir   [j2 i2]]])
                               (range-2d from1 to1)
                               (range-2d from2 to2)))))
          {}
          [[[\N [50 0] [99 0]]      [\E [0 150] [0 199]]]   ;; 1->6
           [[\W [50 0] [50 49]]     [\E [0 149] [0 100]]]   ;; 1->5
           [[\E [149 0] [149 99]]   [\W [99 149] [99 100]]] ;; 2->4
           [[\S [100 49] [149 49]]  [\W [99 50] [99 99]]] ;; 2->3
           [[\N [100 0] [149 0]]    [\N [0 199] [49 199]]]  ;; 2->6
           [[\W [50 50] [50 99]]  [\S [0 100] [49 100]]]  ;; 3->5
           [[\E [99 50] [99 99]]  [\N [100 49] [149 99]]] ;; 3->2
           [[\S [50 149] [99 149]]  [\W [49 150] [49 199]]] ;; 4->6
           [[\E [99 100] [99 149]]  [\W [149 49] [149 0]]]  ;; 4->2
           [[\N [0 100] [49 100]]   [\E [50 50] [50 99]]] ;; 5->3
           [[\W [0 100] [0 149]]    [\E [50 49] [50 0]]]    ;; 5->1
           [[\E [49 150] [49 199]]  [\N [50 149] [99 149]]] ;; 6->4
           [[\W [0 150] [0 199]]    [\S [50 0] [99 0]]]     ;; 6->1
           [[\S [0 199] [49 199]]   [\S [100 0] [149 0]]]]  ;; 6->2
          ))

(defn wrap-portal [dir pos] (get portals [dir pos]))

(declare redraw!)

(defn simulate' [[m instructions]]
  (loop [position (starting-position m)
         direction starting-direction
         instructions instructions]
    (let [[instruction & instructions] instructions]
      (cond
        (number? instruction)
        (let [[new-position new-direction]
              (reduce (fn [[position direction] _]
                        (let [[new-direction new-position]
                              (or (wrap-portal direction position)
                                  [direction
                                   (m+ position (dir->delta direction))])]
                          (case (get-in m new-position)
                            \# [position direction]
                            \. [new-position new-direction])))
                      [position direction]
                      (range instruction))]
          (recur
           new-position
           new-direction
           instructions))

        (char? instruction)
        (recur position
               (case instruction
                 \R (cw direction)
                 \L (ccw direction))
               instructions)


        :else
        (let [[j i] position]
          [(+ (* 1000 (inc j))
              (* 4 (inc i))
              ({\E 0, \S 1, \W 2, \N 3} direction))])))))

(def !pos (atom (starting-position m)))
(def !dir (atom \E))

(defn move! []
  (let [new-position (wrap m (m+ @!pos (dir->delta @!dir))
                           @!dir)]
    (when (= \. (get-in m new-position))
      (reset! !pos new-position))))

(defn move!' []
  (let [[new-direction
         new-position]
        (or (wrap-portal @!dir @!pos)
            [@!dir
             (m+ @!pos (dir->delta @!dir))])]
    (when (= \. (get-in m new-position))
      (reset! !pos new-position)
      (reset! !dir new-direction))))


(require '[io.github.humbleui.ui :as ui]
         '[io.github.humbleui.paint :as paint])

(defn redraw! [pos]
  (def ui
    (ui/key-listener
     {:on-key-down #(case (:key %)
                      :left (swap! !dir ccw)
                      :right (swap! !dir cw)
                      :up   (move!'))}
     (ui/with-bounds ::bounds
         (ui/dynamic
          ctx [height (:height (::bounds ctx))
               width (:width (::bounds ctx))
               #_#_pos @!pos]
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
  (def animate (future (simulate' m+instructions)))
  (future-cancel animate)
  (redraw! [0 0])

  (simulate' m+instructions)

  (ui/start-app!
   (ui/window
    {:title "Viz"}
    #'ui))
  nil)

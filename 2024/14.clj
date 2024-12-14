(require '[clojure.string :as str])

(def p+vs
  (->> "2024/14.in"
       (slurp)
       (str/split-lines)
       (mapv (fn [line]
               (->> line
                    (re-seq #"-?\d+")
                    (mapv parse-long)
                    (partition 2)
                    (into [] (map vec)))))))

(do (def w 101) (def h 103))

(defn positions-at-seconds [seconds]
  (map (fn [[[px py] [vx vy]]]
         [(mod (+ px (* vx seconds)) w)
          (mod (+ py (* vy seconds)) h)])
       p+vs))

(defn position->quadrant [[px py]]
  (let [max-w (dec w), max-h (dec h)
        hmax-w (/ max-w 2), hmax-h (/ max-h 2)]
    (cond
      (and (<= 0 px (dec hmax-w))
           (<= 0 py (dec hmax-h)))
      0

      (and (<= (inc hmax-w) px max-w)
           (<= 0 py (dec hmax-h)))
      1

      (and (<= 0 px (dec hmax-w))
           (<= (inc hmax-h) py max-h))
      2

      (and (<= (inc hmax-w) px max-w)
           (<= (inc hmax-h) py max-h))
      3)))

(reduce *
        (-> (group-by position->quadrant (positions-at-seconds 100))
            (dissoc nil)
            (update-vals count)
            (vals)))
;; => 230461440

(import '[javax.swing JFrame JPanel]
        '[java.awt Graphics]
        '[java.awt.image BufferedImage])

(defonce *seconds (atom 0))
(defonce image (BufferedImage. w h BufferedImage/TYPE_INT_ARGB))

(def white (java.awt.Color. 255 255 255 255))
(def black (java.awt.Color. 0   0   0   255))

(defn redraw-image []
  (doto (.getGraphics image)
    (.setColor white)
    (.fillRect 0 0 w h)
    (.dispose))
  (let [positions (into #{} (positions-at-seconds @*seconds))]
    (doall
     (for [x (range 101), y (range 103)]
       (do
         (when (contains? positions [x y])
           (.setRGB image x y (.getRGB black)))
         nil))))
  image)

(defn create-panel [image]
  (proxy [JPanel] []
    (paintComponent [^Graphics g]
      (proxy-super paintComponent g)
      (.drawImage g image 0 0 this))))

(defonce frame (JFrame. "Image Drawer"))

(defn show-window []
  (let [panel (create-panel image)]
    (.add frame panel)
    (.setSize frame w h)
    (.setVisible frame true)))

(defonce *speed (atom 1))
(defonce *sleep-ms (atom 120))

(comment
  (show-window)

  (do (redraw-image) (.repaint frame))

  (def keep-drawing
    (future
      (loop []
        (println @*seconds)
        (redraw-image)
        (.repaint frame)
        (swap! *seconds + @*speed)
        (Thread/sleep @*sleep-ms)
        (recur))))

  (reset! *seconds 0)
  (reset! *seconds 103) ;; first vertical
  (reset! *seconds 204) ;; second vertical
  (reset! *seconds 179) ;; first horizontal
  (reset! *seconds 206)
  (reset! *speed 101)
  (reset! *speed 1)
  (reset! *speed -1)
  (reset! *speed 0)
  (reset! *sleep-ms 100)
  (reset! *sleep-ms 200)
  (reset! *sleep-ms 500)
  (reset! *sleep-ms 1000)

  (swap! *seconds + -101)

  (future-cancel keep-drawing)

  @*seconds
  ;; => 6668

  nil)

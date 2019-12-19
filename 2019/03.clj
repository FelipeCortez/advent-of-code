(ns crossed-wires
  (:require [clojure.string :as str]))

(def delta-ops {:R [:x +]
                :L [:x -]
                :U [:y +]
                :D [:y -]})

(def other-coord {:x :y, :y :x})

(defn split-commas [s] (str/split s #","))

(defn segment-str->segment [segment-str]
  {:dir   (keyword (subs segment-str 0 1))
   :delta (Integer/parseInt (subs segment-str 1))})


(def wires (->> (slurp "03.in")
                str/split-lines
                (mapv split-commas)))

(defn relative->absolute [segments]
  (loop [pos      {:x 0 :y 0}
         segments segments
         result   {:h [], :v []}
         sum      0]
    (if-not (seq segments)
      result
      (let [segment   (first segments)
            [coord f] (delta-ops (:dir segment))
            new-pos (merge-with f pos {coord (:delta segment)})
            new-sum (+ sum (Math/abs (- (coord new-pos) (coord pos))))]
        (recur new-pos
               (rest segments)
               (update result (coord {:x :h, :y :v})
                       #(conj % [(pos (other-coord coord)) (map coord [pos new-pos]) sum]))
               new-sum)))))

(defn between? [from to n]
  (let [[from to] (sort [from to])]
    (and (<= from n to) n)))

(defn crosses?
  [[fixed1 [from1 to1]]
   [fixed2 [from2 to2]]]
  (and (not= [0 0] [fixed1 fixed2])
       (between? from1 to1 fixed2)
       (between? from2 to2 fixed1)
       {:x fixed2 :y fixed1}))

(defn overlaps?
  [segment1 segment2]
  (let [[fixed1 from-to1] segment1
        [fixed2 from-to2] segment2
        from-to1 (sort from-to1)
        from-to2 (sort from-to2)
        [before after] (sort-by first [from-to1 from-to2])]
    (and (= fixed1 fixed2)
         (<= (- (max (first from-to1) (first from-to2))
                (min (second from-to1) (second from-to2))) 0))))

(def segments (mapv (fn [wire] (relative->absolute (mapv segment-str->segment wire)))
                    wires))

;;; part 1
(apply min (map (fn [{:keys [x y]}] (+ (Math/abs x) (Math/abs y)))
                (let [[{hs1 :h vs1 :v} {hs2 :h vs2 :v}] segments
                      crossings (fn [hs vs] (for [h hs v vs :let [crosses (crosses? h v)] :when crosses]
                                              crosses))]
                  (concat (crossings hs1 vs2) (crossings hs2 vs1)))))

;;; part 2
(defn crossings-delay [hs vs]
  (for [[fixed1 [from1 to1] sum1 :as h] hs
        [fixed2 [from2 to2] sum2 :as v] vs
        :let [crosses (crosses? h v)] :when crosses]
    (+ sum1 (Math/abs (- from1 fixed2))
       sum2 (Math/abs (- from2 fixed1)))))

(apply min (let [[{hs1 :h vs1 :v} {hs2 :h vs2 :v}] segments]
             (concat (crossings-delay hs1 vs2) (crossings-delay hs2 vs1))))

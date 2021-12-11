(def example "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(->> (slurp "08.in")
     (clojure.string/split-lines)
     (map (fn [line]
            (->> (clojure.string/split line #"\s+")
                 (drop-while (partial not= "|"))
                 rest
                 (filter #(#{7 4 3 2} (count %))))))
     (flatten)
     (count))

(defn fake-freq [s]
  (clojure.set/map-invert
   (filter (comp #{4 6 9} second)
           (frequencies (clojure.string/replace s #"\s+" "")))))

(defn freqs [s]
  (reduce (fn [acc x]
            (if (#{7 4 3 2} (count x))
              (conj acc [(count x) (set x)])
              acc))
          {}
          (clojure.string/split s #"\s+")))

(def chars->num
  {#{\a \b \c \e \f \g}    0
   #{\c \f}                1
   #{\a \c \d \e \g}       2
   #{\a \c \d \f \g}       3
   #{\b \c \d \f}          4
   #{\a \b \d \f \g}       5
   #{\a \b \d \e \f \g}    6
   #{\a \c \f}             7
   #{\a \b \c \d \e \f \g} 8
   #{\a \b \c \d \f \g}    9})

(defn b [s] (get (fake-freq s) 6))
(defn e [s] (get (fake-freq s) 4))
(defn f [s] (get (fake-freq s) 9))
(defn c [s] (first (clojure.set/difference
                    (get (freqs s) 2)
                    (set [(f s)]))))
(defn a [s] (first (clojure.set/difference
                    (->> (clojure.string/split s #"\s+")
                         (filter #(= 3 (count %)))
                         first set)
                    (set ((juxt c f) s)))))
(defn d [s] (first (clojure.set/difference
                    (->> (clojure.string/split s #"\s+")
                         (filter #(= 4 (count %)))
                         first set)
                    (set [(b s) (c s) (f s)]))))
(defn g [s] (first (clojure.set/difference
                    #{\a \b \c \d \e \f \g}
                    (set ((juxt a b c d e f) s)))))

(->> (slurp "08.in")
     (clojure.string/split-lines)
     (map (fn [line]
            (let [[former latter] (clojure.string/split line #" \| ")]
              (Integer/parseInt
               (apply str
                      (map (comp (reduce-kv (fn [m k v]
                                              (assoc m
                                                     (set (map #(eval `(~(symbol (str %))
                                                                        ~former))
                                                               k))
                                                     v)) {} chars->num)
                                 set)
                           (clojure.string/split latter #"\s+")))
               10))))
     (reduce +))

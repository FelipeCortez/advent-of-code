(def example "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

@(def open->close {\{ \}, \[ \], \( \), \< \>})

(def opens? (set (keys open->close)))
(def closes? (set (vals open->close)))

(defn middle [coll] (nth coll (/ (dec (count coll)) 2)))

(let [parsed
      (->> (slurp "10.in")
           clojure.string/split-lines
           (map (fn [line]
                  (reduce (fn [acc char]
                            (if (opens? char)
                              (conj acc char)
                              (if (= char (open->close (peek acc)))
                                (pop acc)
                                (reduced {:illegal char}))))
                          [] line))))]
  [(->> parsed
        (filter map?)
        (map (comp {\) 3, \] 57, \} 1197, \> 25137} :illegal))
        (reduce +))
   (->> parsed
        (filter vector?)
        (map (fn [pending]
               (reduce (fn [score char] (->> (* 5 score) (+ ({\( 1, \[ 2, \{ 3, \< 4} char))))
                       0
                       (reverse pending))))
        sort
        middle)])

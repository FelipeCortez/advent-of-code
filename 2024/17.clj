(require '[clojure.string :as str])

(def base-computer {:ip 0 :A 0 :B 0 :C 0 :program [] :output []})

(defn combo [computer operand]
  (case operand
    (0 1 2 3) operand
    4         (:A computer)
    5         (:B computer)
    6         (:C computer)))

(defn ip-inc [computer] (update computer :ip + 2))

(defn compute [computer]
  (loop [computer computer]
    (let [{:keys [ip program A B C]} computer
          opcode (get program ip), operand (get program (inc ip))]
      (if-not opcode
        computer
        (recur
         (case opcode
           0 (assoc  (ip-inc computer) :A (bit-shift-right A (combo computer operand)))
           1 (assoc  (ip-inc computer) :B (bit-xor B operand))
           2 (assoc  (ip-inc computer) :B (bit-and (combo computer operand) 7))
           3 (assoc  computer :ip (if (zero? A) (+ ip 2) operand))
           4 (assoc  (ip-inc computer) :B (bit-xor B C))
           5 (update (ip-inc computer) :output conj (bit-and (combo computer operand) 7))
           6 (assoc  (ip-inc computer) :B (bit-shift-right A (combo computer operand)))
           7 (assoc  (ip-inc computer) :C (bit-shift-right A (combo computer operand)))))))))

(->> {:program [2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0] :A 30878003}
     (merge base-computer)
     compute :output (str/join ","))
;; => "7,1,3,7,5,1,0,3,4"

(first
 (filter (fn [A]
           (= [2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0]
              (->> {:program [2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0] :A A}
                   (merge base-computer)
                   compute
                   :output)))
         (range 8r5322350130000000 8r5322350137600000)))

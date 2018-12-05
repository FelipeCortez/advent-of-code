(ns alchemical-reduction)

;; part 1
(def case-distance (- (int \A) (int \a)))

(defn eq-upper-or-lower [x y]
  (or (= (+ (int x) case-distance) (int y))
      (= (- (int x) case-distance) (int y))))

(def polymer (-> "5.in" slurp clojure.string/trim-newline))

(defn react [polymer]
  (reduce (fn [coll x]
            (if (and (seq coll) (eq-upper-or-lower x (first coll)))
              (rest coll)
              (cons x coll)))
          []
          polymer))

(count (react polymer))

;; part 2
(defn lowercase-eq [& args]
  (apply = (map clojure.string/lower-case args)))

(defn remove-char [char string]
  (remove #(lowercase-eq char %) string))

(def a-to-z (map char (range (int \a) (inc (int \z)))))

(apply min (map #(->> polymer
                      (remove-char %)
                      react
                      count)
                a-to-z))

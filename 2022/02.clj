(ns rock-paper-scissors
  (:require [clojure.string :as string]))

(def game (string/split-lines (slurp "02.in")))

(def game->score-pt1
  {"A X" 4
   "B X" 1
   "C X" 7
   "A Y" 8
   "B Y" 5
   "C Y" 2
   "A Z" 3
   "B Z" 9
   "C Z" 6})

(reduce + (map game->score-pt1 game))

(def game->score-pt2
  {"A X" 3
   "B X" 1
   "C X" 2
   "A Y" 4
   "B Y" 5
   "C Y" 6
   "A Z" 8
   "B Z" 9
   "C Z" 7})

(reduce + (map game->score-pt2 game))

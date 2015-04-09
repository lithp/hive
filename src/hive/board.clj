(ns hive.board
  (:refer-clojure :rename {peek cpeek, pop cpop})
  (:require [hive.coord :as coord]))

(defn piece [color insect] {:color color :insect insect})

(def EMPTY {})

(defn add [board to piece]
  (if piece
    (assoc board to (conj (board to) piece))
     board))

(defn pop [board from]
  (let [from-stack (board from)]
    (if (seq (rest from-stack))
      (assoc board from (rest from-stack))
      (dissoc board from))))

(defn peek [board from]
  (cpeek (board from)))

(defn move [board from to]
  (let [piece (peek board from)]
    (assert piece [board from])
    (add (pop board from) to piece)))

(defn occupied-neighbors [board coord]
  (filter board (coord/neighbors coord)))

(defn unoccupied-neighbors [board coord]
  (remove board (coord/neighbors coord)))

(defn connected? [board coord]
  (if (board coord)
    coord
    (first (occupied-neighbors board coord))))

(defn stack-size [board coord] (count (board coord)))

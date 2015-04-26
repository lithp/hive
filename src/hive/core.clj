(ns hive.core
  (:gen-class)
  (:require [clojure.set :as set]
            [hive.board :as board]
            [hive.coord :as coord]))

(def starting-pieces
  {:ant 3 :grasshopper 3 :beetle 2 :spider 2 :ladybug 1 :mosquito 1 :queen 1 :pullbug 1})

(defn available-insects [board color]
  "All unspawned pieces color has left to place"
  (->> (vals board)
       (flatten)
       (filter #(= color (:color %)))
       (map (fn [piece] {(:insect piece) 1}))
       (apply merge-with - starting-pieces)))

(defn free-to-move? [board from to]
  "When going from a->b and flanked by c, d
   
   we are only free-to-move if the tallest of a, b (not including the beetle)
   is as least as tall as the shortest of c, d.
  "
 (let [direction (coord/sub to from)
       stack-size (partial board/stack-size board)
       a-b [(- (stack-size from) 1) (stack-size to)]
       c-d (map stack-size (coord/adjacent-coords from direction))]
  (>= (apply max a-b) (apply min c-d))))

; a board is a map {coord -> [piece]}

(defn grasshopper-moves [board coord]
  "Starting at coord, a grasshopper moves in a single direction until the first
   empty space. It must move over at least one occupied space"
  (keep (fn [direction]
          (when (contains? board (coord/add coord direction))
            (loop [coord coord]
              (if (contains? board coord)
                (recur (coord/add coord direction))
                coord))))
    coord/directions))

(defn beetle-moves [board coord]
  "The beetle may move one space in any direction, including occupied ones."
    (filter (every-pred (partial free-to-move? board coord)
                        (partial board/connected? (board/pop board coord)))
            (coord/neighbors coord)))

(defn queen-moves [board coord]
  "The queen may move in any unoccupied direction"
  (remove (partial contains? board) (beetle-moves board coord)))

(def pillbug-moves queen-moves)

(defn ant-moves [board coord]
  "Ants move like a queen but repeatedly.

   Do a BFS of available moves.
  "
  (loop [seen #{coord} queue (conj clojure.lang.PersistentQueue/EMPTY coord)]
    (if (empty? queue)
      (disj seen coord) ; you can't move to where you already were
      (let [node (peek queue)
            children (remove seen (queen-moves (board/move board coord node) node))]
        (recur (into seen children) (into (pop queue) children))))))

(defn spider-moves [board coord]
  "Spiders move like queens but exactly three spaces

   Do a BFS for all 3-length paths which don't backtrack (e.g. A -> B -> A)
   TODO: May a spider end where it started? The way this is implemented says yes.
  "
  (letfn [(explore-path [path]
            (for [move (queen-moves (board/move board coord (peek path)) (peek path))
                  :when (not= move (peek (pop path)))]
              (conj path move)))
          (search [depth paths]
           (if (= 3 depth)
               paths
               (recur (inc depth) (mapcat explore-path paths))))]
    (set (map peek (search 0 [[coord]])))))

(defn ladybug-moves [board coord]
  "Ladybugs move up, then one along the top, then one down"
  (->> coord
       (board/occupied-neighbors board)
       (mapcat (partial board/occupied-neighbors (dissoc board coord)))
       (mapcat (partial board/unoccupied-neighbors board))
       (set)))

(declare available-moves)

(defn mosquito-moves [board coord]
  "The mosquito may move like any of the pieces surrounding it"
  (apply set/union
    (let [insects (distinct (map (comp :insect first board)
                                 (board/occupied-neighbors board coord)))]
      (for [insect insects :when (not= :mosquito insect)]
        (available-moves board insect coord)))))

(defn available-moves [board insect coord]
  (set
    ((insect {:grasshopper grasshopper-moves
              :beetle beetle-moves
              :queen queen-moves
              :ant ant-moves
              :spider spider-moves
              :ladybug ladybug-moves
              :pillbug pillbug-moves
              :mosquito mosquito-moves})
      board coord)))

(defn iterate-until-fixed [f initial]
  (loop [accum initial]
    (let [next-accum (f accum)]
      (if (= accum next-accum)
          accum
          (recur next-accum)))))

(defn one-hive-movable-pieces [board]
  "Pieces which connect two separate hives may not be moved, so:
     Pieces which have only one neighbor may move
     Pieces which are part of a cycle may move
       Except for those which are neighbors of a non-cyclic part of the board

   Pieces may also move if they are part of a stack which is higher than 1."
  (letfn [(num-neighbors [board coord] (count (board/occupied-neighbors board coord)))
          (leaf-nodes [board] (filter #(= 1 (num-neighbors board %))
                                      (keys board)))
          (without-leafs [board] (apply dissoc board (leaf-nodes board)))]
  (let [stacks (map key (filter (comp (partial < 1) count val) board))
        orig-leafs (leaf-nodes board)
        minimal-board (iterate-until-fixed without-leafs board)
        ; remove all coords which have a neighbor not in minimal board
        excluding-leaf-neighbors (filter #(every? minimal-board (board/occupied-neighbors board %))
                                   (keys minimal-board))]
    (set (concat stacks orig-leafs excluding-leaf-neighbors)))))

; the first move is a special case, player 1 spawns on the origin,
; player 2 spawns anywhere
(defn spawn-locations [board color]
  "Every position which is connected to the hive yet only next to pieces with :color color"
  (letfn [(stack-color [pieces] (:color (first pieces)))
          (emit-neighbors [[coord color]]
            (for [neighbor (board/unoccupied-neighbors board coord)]
              {color #{neighbor}}))]
  (let [colored-coords (map (fn [[coord pieces]] [coord (stack-color pieces)])
                            board)
        neighbors (mapcat emit-neighbors colored-coords)
        by-color (apply merge-with set/union neighbors)]
    (apply set/difference (by-color color) (vals (dissoc by-color color)))
)))

(defn pillbug-throws [board coord]
  "What are all the special moves the pillbug at coord can make?

   A pillbug may only move pieces which are not stacked and which won't break
   the hive by being moved. It may move pieces into any empty location adjacent
   to themselves."
  (for [neighbor (board/occupied-neighbors board coord)
        empty-spot (board/unoccupied-neighbors board coord)
        :when (and (contains? (one-hive-movable-pieces board) neighbor)
                   (= 1 (board/stack-size board neighbor)))]
    [neighbor empty-spot]))

(defn all-moves [board color]
  "A sequence of all moves color may make this turn of the form [source dest].

   For each movable piece all the places it may move to."
  (let [movable-coords (one-hive-movable-pieces board)
        my-coords (filter (comp (partial = color) :color first board) movable-coords)]
    (apply concat
    (for [coord my-coords
          :let [insect (:insect (first (board coord)))]]
      (for [dest (available-moves board insect coord)]
        [coord dest])))))

(defn next-color [color]
  (color {:black :white
          :white :black}))

(defn all-next-boards [board color last-moved]
  "A sequence of all boards which could result from color moving this turn
   returns vectors of [board color last-moved]

   last-moved is the piece last moved/thrown by a player, it may not be moved
   or thrown this turn."
  (lazy-cat

    (for [coord (spawn-locations board color)
          insect (keys (available-insects board color))
          :let [piece (board/piece color insect)]]
      [(board/add board coord piece) (next-color color) coord])

    (let [moves (all-moves board color)]
      (for [[from to] moves :when (not= from last-moved)]
        [(board/move board from to) (next-color color) to]))))

(defn game-won? [board]
  "Returns nil if the game is not finished.
   Returns :draw if both queens are surrounded
   Returns a color if it has won by surrounding the other queen."
  (let [queens (keep (fn [[coord stack]] (when (= :queen (:insect (last stack)))
                                             coord))
                       board)
        surrounded? (fn [coord] (every? (partial contains? board) (coord/neighbors coord)))
        loser-coords (filter surrounded? queens)
        loser-colors (set (map (comp :color last board) loser-coords))
        all-colors (set (map (comp :color last board) queens))

        winners (set/difference all-colors loser-colors)]
    (assert (>= 2 (count queens)))
    (cond (= 0 (count loser-colors)) nil
          (= 2 (count loser-colors)) :draw
          :default (first winners))))

(defn -main
  "Plays Hive!"
  [& args]
  (println "Hello, World!"))

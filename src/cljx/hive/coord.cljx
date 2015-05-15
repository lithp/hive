(ns hive.coord)

; the coordinates are "cube coordinates" from 
; http://www.redblobgames.com/grids/hexagons/
;
; x + y + z = 0

(defn valid-coord?
  "Each location has a single canonical representation"
  [coord] (= 0 (reduce + coord)))

(def add (comp vec (partial map +)))
(def sub (comp vec (partial map -)))

; these are mostly for testing, but often convenient to have
(def origin [0 0 0])
(def up [0 1 -1])
(def up-right [1 0 -1])
(def down-right [1 -1 0])
(def down (sub origin up))
(def down-left (sub origin up-right))
(def up-left (sub origin down-right))

(def upup (add up up))
(def downdown (add down down))

(def directions [[-1 0 1]
                 [-1 1 0]
                 [0 -1 1]
                 [0 1 -1]
                 [1 0 -1]
                 [1 -1 0]])

(defn neighbors [coord] (map (partial add coord) directions))

(defn adjacent-directions [direction]
  "Return the two directions next to the given direction"
  (letfn [(swap [direction left right]
            (let [left-idx (.indexOf direction left)
                  right-idx (.indexOf direction right)]
              (assoc direction left-idx right right-idx left)))]
    (assert (valid-coord? direction) direction)
    [(swap direction 0 1) (swap direction 0 -1)])) 

(defn adjacent-coords [from direction]
  (map (partial add from) (adjacent-directions direction)))

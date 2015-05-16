(ns hive.core
  (:require [hive.board :as board]
            [hive.coord :as coord]
            [monet.canvas :as canvas]))

(enable-console-print!)

(def board (atom board/EMPTY))

(def sqrt (.-sqrt js/Math))

(defn draw-hex [ctx width x y]
  ; draws a hexagon centered at x, y
  (let [qw (/ width 4)
        hw (/ width 2)
        height (* (sqrt 3) hw) ; sqrt(3)/2 * width
        qh (/ height 4)
        hh (/ height 2)]
    (canvas/begin-path ctx)
    (canvas/move-to ctx (- x hw) y) ; left
    (canvas/line-to ctx (- x qw) (- y hh)) ; up-left
    (canvas/line-to ctx (+ x qw) (- y hh)) ; up-right
    (canvas/line-to ctx (+ x hw) y) ; right
    (canvas/line-to ctx (+ x qw) (+ y hh)) ; down-right
    (canvas/line-to ctx (- x qw) (+ y hh)) ; down-left
    (canvas/close-path ctx)
    (canvas/stroke ctx)))

(defn draw-rect-at [ctx x y]
  (canvas/fill-rect ctx {:x x :y x :w 10 :h 10}))

(defn hex-coord-to-canvas-coord [width [x y z :as coord]]
  ; the origin maps to 0, 0
  ; stolen from http://www.redblobgames.com/grids/hexagons/#hex-to-pixel
  (let [q x, r z]
    [(* width (/ 3 4) q)
     (* width (/ (sqrt 3) 2) (+ r (/ q 2)))]))

(defn offset-fn [canvas-elem]
  (let [width (.-width canvas-elem)
        height (.-height canvas-elem)]
    (fn [[x y]] [(+ (/ width 2) x)
                 (+ (/ height 2) y)])))

(defn draw-board [board canvas-elem]
  (let [ctx (canvas/get-context canvas-elem "2d")
        coord-fn (comp (offset-fn canvas-elem)
                       (partial hex-coord-to-canvas-coord 50))]
    (doseq [x (range 10), y (range 10)
            :let [coord (nth (iterate (partial coord/add coord/down-right) coord/origin) x)
                  coord (nth (iterate (partial coord/add coord/down) coord) y)]]
      (apply draw-hex ctx 50 (coord-fn coord)))))

(defn draw [canvas-elem]
  (draw-board board canvas-elem))

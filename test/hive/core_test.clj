(ns hive.core-test
  (:require [clojure.test :refer :all]
            [hive.core :refer :all]
            [hive.coord :as coord :refer [origin up up-right down-right down down-left up-left]]))

(defn make-board [& coords]
  (reduce #(board-add %1 %2 (piece :unknown :unknown)) {} coords))

(deftest board-regression-tests
  (testing "board-pop dissoc's if there is nothing left"
    (is (= {} (board-pop (make-board origin) origin)))))

(deftest freedom-of-movement
  (testing "Simple case, if there is nobody in the way movement is allowed"
    (is (free-to-move? {} origin (coord/add origin up)))))
  (testing "If there is one piece in the way, movement is still allowed"
    (is (free-to-move? (make-board (coord/add origin up)) origin (coord/add origin up-right))))
  (testing "If there are two pieces in the way, we are not free to move"
    (let [board (make-board (coord/add origin up) (coord/add origin down-right))]
      (is (not (free-to-move? board origin (coord/add origin up-right))))))
  (testing "If the destination is raised we are freed to move"
    (let [board (make-board (coord/add origin up)
                            (coord/add origin down-right)
                            (coord/add origin up-right))]
      (is (free-to-move? board origin (coord/add origin up-right)))))

(deftest moving
  (testing "simple case"
    (let [board (make-board origin)]
      (= (make-board up) (move board origin up))))
  (testing "works when origin and dest are same coord"
    (let [board (make-board origin origin (coord/add origin up))]
      (= board (move board origin origin)))))

(deftest grasshopper-motion
  (testing "cannot move when not surrounded"
    (is (empty? (grasshopper-moves {} origin))))
  (testing "will jump ovr an arbitrary number of pieces"
     (let [distance (take 5 (iterate (partial coord/add up) origin))
           board (apply make-board distance)]
       (is (= (grasshopper-moves board origin)
              [(coord/add up (last distance))])))))

(defn test-standard-motions [move-fn]
 (testing "will not move away from the hive"
   (let [board (make-board origin up)
         without-piece (make-board origin)]
     (is (every? (partial connected? without-piece) (move-fn board up)))))
  ; TODO: fix me!
  ;(testing "can only move along pieces, not jump from one connection to another"
  ; (let [one-up (coord/add up origin)
  ;       two-up (coord/add up one-up)
  ;       three-up (coord/add up two-up)
  ;       board (make-board origin three-up)]
  ;   (is (not-any? #{two-up} (move-fn board one-up)))))
)

(deftest beetle-motion
  (test-standard-motions beetle-moves)
  (testing "can crawl from stack"
    (let [board (make-board origin origin)]
      (is (= #{up up-right down-right down down-left up-left}
             (into #{} (beetle-moves board origin))))))
  (testing "simple case"
    (let [board (make-board origin up)]
      (is (= #{origin up-right up-left}
             (into #{} (beetle-moves board up))))))
  (testing "obeys freedom to move"
    (let [board (make-board origin up down-right down-left)]
      (is (= #{up down-right down-left}
             (into #{} (beetle-moves board origin))))))
)

(deftest queen-motion
  (test-standard-motions queen-moves)
  (testing "simple case"
    (let [board (make-board origin up)]
      (is (= #{up-right up-left}
             (into #{} (queen-moves board up))))))
  (testing "obeys freedom to move"
    (let [board (make-board origin up down-right down-left)]
      (is (empty? (queen-moves board origin)))))
)

(deftest pillbug-motion
  (test-standard-motions pillbug-moves)
)

(deftest ant-motion
  (test-standard-motions ant-moves)
  (testing "can move to many locations"
    (let [board (make-board origin up)]
      (is (= 5 (count (ant-moves board up))))))
)

(deftest spider-motion
  (testing "simple case"
    (let [board (make-board origin up)]
      (is (= #{down}
             (into #{} (spider-moves board up))))))
)

(deftest one-hive
  (testing "simple cases"
    (let [board (make-board origin up)]
      (is (= 2 (count (one-hive-movable-pieces board)))))
    (let [board (make-board origin up (coord/add up up))]
      (is (= 2 (count (one-hive-movable-pieces board))))))
  (testing "rings"
    (let [board (make-board up up-right down-right down down-left up-left)]
      (is (= 6 (count (one-hive-movable-pieces board)))))
    (let [board (make-board origin up up-right down-right down down-left up-left)]
      (is (= 7 (count (one-hive-movable-pieces board)))))
    (let [board (make-board up (coord/add up up) up-right down-right down down-left up-left)]
      (is (= 6 (count (one-hive-movable-pieces board)))))))

(deftest spawns
  (testing "simple cases"
    (let [board (board-add empty-board origin (piece :white :spider))]
      (is (= 0 (count (spawn-locations board :black))))
      (is (= 6 (count (spawn-locations board :white)))))))

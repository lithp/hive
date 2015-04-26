(ns hive.core-test
  (:require [clojure.test :refer :all]
            [hive.core :refer :all]
            [hive.board :as board]
            [hive.coord :as coord :refer [origin up up-right down-right down
                                          down-left up-left upup downdown]]))

(defn make-board [& specs]
  (letfn [(add-args [[coord color insect :as all]]
                      (if (every? number? all)
                        [all (board/piece :unknown :unknown)]
                        [coord (board/piece (or color :unknown) (or insect :unknown))]))]
    (reduce #(apply board/add %1 (add-args %2)) board/EMPTY specs)))

(deftest board-regression-tests
  (testing "board/pop dissoc's if there is nothing left"
    (is (= {} (board/pop (make-board origin) origin)))))

(deftest piece-math
  (testing "With an empty board, you have all pieces available to you"
    (is (= starting-pieces (available-insects board/EMPTY :white))))
  (testing "With a fuller board subtracts correctly"
    (is (= (assoc starting-pieces :ant 0)
           (available-insects (make-board [origin :white :ant]
                                         [up :white :ant]
                                         [down :white :ant])
                             :white))))
  (testing "Stacks of pieces are correctly accounted for"
    (is (= (assoc starting-pieces :beetle 0
                                  :mosquito 0)
           (available-insects (make-board [origin :white :beetle]
                                         [origin :white :beetle]
                                         [up :white :mosquito]
                                         [up :black :beetle])
                             :white)))))

(deftest freedom-of-movement
  (testing "Simple case, if there is nobody in the way movement is allowed"
    (is (free-to-move? {} origin up))))
  (testing "If there is one piece in the way, movement is still allowed"
    (is (free-to-move? (make-board up) origin up-right)))
  (testing "If there are two pieces in the way, we are not free to move"
    (let [board (make-board up down-right)]
      (is (not (free-to-move? board origin up-right)))))
  (testing "If the destination is raised we are freed to move"
    (let [board (make-board up down-right up-right)]
      (is (free-to-move? board origin up-right))))

(deftest moving
  (testing "simple case"
    (let [board (make-board origin)]
      (= (make-board up) (board/move board origin up))))
  (testing "works when origin and dest are same coord"
    (let [board (make-board origin origin up)]
      (= board (board/move board origin origin)))))

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
     (is (every? (partial board/connected? without-piece) (move-fn board up)))))
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
      (is (= 5 (count (ant-moves board up)))))))

(deftest spider-motion
  (testing "simple case"
    (let [board (make-board origin up)]
      (is (= 1 (count (spider-moves board up)))))))

(deftest ladybug-motion
  (testing "can't move when there isn't a second piece to crawl onto"
    (let [board (make-board origin up)]
      (is (empty? (ladybug-moves board up)))))
  (testing "must crawl at least one piece"
    (let [board (make-board origin up upup)]
      (is (= (disj (into #{} coord/directions) up)
             (into #{} (ladybug-moves board upup)))))))

(deftest mosquito-motion
  (testing "simple case, can steal from ant"
    (let [board (make-board [origin :white :ant] [up :black :mosquito])]
      (is (= 5 (count (mosquito-moves board up))))))
  (testing "immobilized when next to mosquito"
    (let [board (make-board [origin :white :mosquito] [up :black :mosquito])]
      (is (= 0 (count (mosquito-moves board up)))))))

(deftest one-hive
  (testing "simple cases, only leaf nodes may move"
    (let [board (make-board origin up)]
      (is (= 2 (count (one-hive-movable-pieces board)))))
    (let [board (make-board origin up upup)]
      (is (= 2 (count (one-hive-movable-pieces board))))))
  (testing "rings, all pieces not next to a previous leaf may move"
    (let [board (make-board origin up-left up up-right)]
      (is (= 4 (count (one-hive-movable-pieces board)))))
    (let [board (make-board up up-right down-right down down-left up-left)]
      (is (= 6 (count (one-hive-movable-pieces board)))))
    (let [board (make-board origin up up-right down-right down down-left up-left)]
      (is (= 7 (count (one-hive-movable-pieces board)))))
    (let [board (make-board up upup up-right down-right down down-left up-left)]
      (is (= 6 (count (one-hive-movable-pieces board))))))
  (testing "pieces on top of a stack may move")
    (let [board (make-board origin origin up down)]
      (is (= 3 (count (one-hive-movable-pieces board))))))

(deftest spawns
  (testing "simple cases"
    (let [board (make-board [origin :white])]
      (is (= 0 (count (spawn-locations board :black))))
      (is (= 6 (count (spawn-locations board :white)))))
    (let [board (make-board [origin :white] [up :black])]
      (is (= 3 (count (spawn-locations board :black))
               (count (spawn-locations board :white)))))))

(deftest pillbug-special
  (testing "can throw"
    (let [board (make-board origin up)]
      (is (= 5 (count (pillbug-throws board origin))))))
  (testing "does not throw stacks"
    (is (empty? (pillbug-throws (make-board origin up up) origin))))
  (testing "does not throw pieces which would break the hive"
    (is (empty? (pillbug-throws (make-board origin up upup) origin)))))

(deftest all-moves-generation
  (testing "single piece"
    (let [board (make-board [origin :white :queen])]
      (is (= 0 (count (all-moves board :black))))
      (is (= 0 (count (all-moves board :white)))))
    (let [board (make-board [origin :white :queen]
                            [origin :black :beetle])]
      (is (= 6 (count (all-moves board :black))))))
  (testing "takes into account hive-movability"
    (let [board (make-board [origin :white :queen]
                            [up :white :beetle]
                            [down :black :queen]
                            [downdown :white :beetle])]
      (is (= 6 (count (all-moves board :white)))))))

(deftest next-board-generation
  (with-redefs [available-insects (constantly {:beetle 1 :pillbug 1})]
    (let [board (make-board [origin :white :queen]
                            [up :black :queen])]
      (comment "This states the queen may be moved on the second turn, even though"
               "the resulting board is equivalent to the starting one")
      (is (= (+ 2 (* 2 3)) (count (all-next-boards board :white nil)))))))

(deftest win-condition
  (testing "game is not won when a queen is not surrounded"
    (let [board (make-board [origin :white :queen] up up-right down-right down down-left)]
      (is (not (game-won? board)))))
  (testing "game is won when a queen is surrounded"
    (let [board (make-board [origin :white :queen]
                            [up :black :queen] up-right down-right down down-left up-left)]
      (is (= :black (game-won? board)))))
  (testing "game is a draw when both queens are surrounded"
    (let [board (make-board [origin :white :queen]
                            [up :black :queen]
                            upup (coord/add up up-left) (coord/add up up-right)
                            up-left up-right down-left down-right down)]
      (is (= :draw (game-won? board))))))

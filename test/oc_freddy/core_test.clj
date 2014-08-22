(ns oc-freddy.core-test
  (:require [clojure.test :refer :all]
            [oc-freddy.core :refer :all]))

(def simple-board
  {:tiles (to-array [{:tile :wall}       {:tile :wall}       {:tile :wall}       {:tile :wall}       {:tile :wall}
                     {:tile :wall}       {:tile :air}        {:tile :air}        {:tile :air}        {:tile :wall}
                     {:tile :wall}       {:tile :hero :id 1} {:tile :tavern}     {:tile :mine}       {:tile :wall}
                     {:tile :wall}       {:tile :air}        {:tile :air}        {:tile :air}        {:tile :wall}
                     {:tile :wall}       {:tile :wall}       {:tile :wall}       {:tile :wall}       {:tile :wall}])
   :size 5})

(deftest safe-coord-test
  (testing "Test safe min coordinates"
    (is (= (safe-min-coord 0) 0))
    (is (= (safe-min-coord -1) 0))
    (is (= (safe-min-coord 10) 10)))
  (testing "Test safe max coordinates"
    (is (= (safe-max-coord 0 10) 0))
    (is (= (safe-max-coord 10 10) 9))
    (is (= (safe-max-coord 9 10) 9))))

(deftest move-and-direction-to-test
  (testing "Basic moves return correct results"
    (is (= (move 100 (make-pos 50 50) :north) (make-pos 49 50)))
    (is (= (move 100 (make-pos 50 50) :south) (make-pos 51 50)))
    (is (= (move 100 (make-pos 50 50) :east)  (make-pos 50 51)))
    (is (= (move 100 (make-pos 50 50) :west)  (make-pos 50 49))))
  (testing "Moves at extremeties"
    (is (= (move 100 (make-pos 0  0)  :north) (make-pos 0  0)))
    (is (= (move 11  (make-pos 10 0)  :south) (make-pos 10 0)))
    (is (= (move 11  (make-pos 0  10) :east)  (make-pos 0  10)))
    (is (= (move 100 (make-pos 0  0)  :west)  (make-pos 0  0))))
  (testing "Direction to and move return the same results"
    (is (= (direction-to (make-pos 50 50) (move 100 (make-pos 50 50) :north)) :north))
    (is (= (direction-to (make-pos 50 50) (move 100 (make-pos 50 50) :south)) :south))
    (is (= (direction-to (make-pos 50 50) (move 100 (make-pos 50 50) :east)) :east))
    (is (= (direction-to (make-pos 50 50) (move 100 (make-pos 50 50) :west)) :west))))

(deftest board-property-test
  (testing "Can move to tests"
    (is (= (can-move-to simple-board (make-pos 0 0)) false))
    (is (= (can-move-to simple-board (make-pos 1 1)) true))
    (is (= (can-move-to simple-board (make-pos 2 1)) true))
    (is (= (can-move-to simple-board (make-pos 2 2)) true))
    (is (= (can-move-to simple-board (make-pos 2 3)) true))))

(deftest ordered-list-test
  (testing "Insert keeps nodes in order"
    (is (=
          (map :score
            (insert-into
              (insert-into
                (insert-into
                  (insert-into [] (make-node nil 1 []))
                  (make-node nil 0 []))
                (make-node nil 3 []))
            (make-node nil 2 [])))
        [0 1 2 3]))))

(deftest node-function-test
  (testing "Distance from start is calculated correctly"
    (is (= (distance-from-start (make-node nil 0 [])) 0))
    (is (= (distance-from-start (make-node nil 0 [(make-pos 0 0)])) 1))
    (is (= (distance-from-start (make-node nil 0 [(make-pos 0 0) (make-pos 0 0)])) 2)))
  (testing "First direction works"
    (is (= :south (first-direction (make-node nil 0 [(make-pos 50 50) (move 100 (make-pos 50 50) :north)]))))
    (is (= :north (first-direction (make-node nil 0 [(make-pos 50 50) (move 100 (make-pos 50 50) :south)]))))
    (is (= :west  (first-direction (make-node nil 0 [(make-pos 50 50) (move 100 (make-pos 50 50) :east)]))))
    (is (= :east  (first-direction (make-node nil 0 [(make-pos 50 50) (move 100 (make-pos 50 50) :west)]))))))

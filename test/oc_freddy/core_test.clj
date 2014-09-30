(ns oc-freddy.core-test
  (:require [clojure.test :refer :all]
            [oc-freddy.core :refer :all]
            [oc-freddy.client :refer :all]))

(def wall {:tile :wall})
(def air {:tile :air})
(def beer {:tile :tavern})
(defn mine
  ([] {:tile :mine})
  ([id] {:tile :mine :of id}))
(defn hero [i] {:tile :hero :id i})

(def simple-board
  {:tiles (to-array
            (with-positions
              [wall     wall     wall     wall     wall     wall
               wall     air      air      air      air      wall
               wall     (mine)   beer     (mine 1) air      wall
               wall     air      air      air      air      wall
               wall     beer     air      air      (hero 4) wall
               wall     wall     wall     wall     wall     wall]))
   :size 6})
(def simple-board-paths (all-paths simple-board))

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
          [0 1 2 3])))
  (testing "Inserts multiple and puts them in order"
    (is (=
          (map :score (insert-into [] (make-node nil 1 [])
                                      (make-node nil 2 [])
                                      (make-node nil 0 [])
                                      (make-node nil 3 [])))
          [0 1 2 3])))
  (testing "Inserts no records and returns existing"
    (let [q [(make-node nil 1 []) (make-node nil 1 []) (make-node nil 1 [])]]
      (is (= (insert-into q) q)))))

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

(deftest simple-path-test
  (testing "Same spot test"
    (let [results   (simple-path simple-board-paths (make-pos 1 1) (make-pos 1 1))
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :stay))
      (is (= distance 0))))
  (testing "One step test"
    (let [results   (simple-path simple-board-paths (make-pos 1 1) (make-pos 1 2))
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 1))))
  (testing "Two step test"
    (let [results   (simple-path simple-board-paths (make-pos 1 1) (make-pos 1 3))
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 2))))
  (testing "Around the corner"
    (let [results   (simple-path simple-board-paths (make-pos 1 2) (make-pos 4 2))
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 7))))
  (testing "Path to mine"
    (let [results   (simple-path simple-board-paths (make-pos 4 2) (make-pos 2 1))
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :north))
      (is (= distance 3))))
  (testing "Path to hero"
    (let [results   (simple-path simple-board-paths (make-pos 3 1) (make-pos 4 4))
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 4)))))

(def bigger-board
  {:tiles (parse-tiles "####    ######            ######    ####$1  $1    ######        ######    $-  $-##          ##    ####    ##          ##                                @4          ##    ##  ##@1[][]  ##  ##    ##          ##        ########        ##      ##  ####      ############      ####  ##  ####            ####            ####    ##    ##                    ##    ##            $1##$-        $-##$-                    $-##$-        $-##$-            ##    ##                    ##    ##    ####            ####            ####  ##  ####      ############      ####  ##      ##        ########        ##          ##@2  ##  ##  [][]  ##  ##    ##                                            ##          ##    ####    ##          ##$-  $2    ######        ######    $3  $-####    ######            ######@3  ####")
   :size 20})
(def bigger-board-paths (all-paths bigger-board))

(deftest bigger-board-test
  (testing "Simple path, bigger board"
    (let [results   (simple-path bigger-board-paths (make-pos 4 8) (make-pos 15 3))
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 4)))))

(deftest with-positions-test
  (testing "With positions works"
    (is (= (:pos (tile-at simple-board (make-pos 2 1))) (make-pos 2 1)))))

(deftest tile-filtering-test
  (testing "Finding all beers works"
    (let [beers (all-beers simple-board)]
      (is (contains? (set beers) (make-pos 2 2)))
      (is (contains? (set beers) (make-pos 4 1)))
      (is (= (count beers) 2))))
  (testing "Finding all mines works"
    (let [mines (capturable-mines simple-board 1)]
      (is (contains? (set mines) (make-pos 2 1)))
      (is (= (count mines) 1)))))

(deftest closest-test
  (testing "Finding closest beer works"
    (is (= (:destination (closest-beer simple-board simple-board-paths (make-pos 1 1))) (make-pos 2 2)))
    (is (= (:destination (closest-beer simple-board simple-board-paths (make-pos 4 2))) (make-pos 4 1))))
  (testing "Finding closest capturable mine works"
    (is (= (:destination (closest-capturable-mine simple-board simple-board-paths (make-pos 1 1) 1)) (make-pos 2 1)))
    (is (= (:destination (closest-capturable-mine simple-board simple-board-paths (make-pos 4 3) 1)) (make-pos 2 1)))
    (is (= (:destination (closest-capturable-mine simple-board simple-board-paths (make-pos 4 3) 2)) (make-pos 2 3)))))

(def hero-board
  {:tiles (to-array
            (with-positions
              [wall     wall     wall     wall     wall    wall     air      wall
               wall     air      air      (mine)   air     (hero 2) air      wall
               wall     air      air      air      air     air      air      wall
               wall     air      wall     beer     air     air      air      wall
               wall     air      wall     air      air     air      air      wall
               wall     air      air      air      air     air      air      wall
               wall     air      air      air      air     air      air      wall
               wall     wall     wall     wall     wall    wall     wall     wall]))
   :size 8})

(def full-life-heroes
  {1 {:life 100} 2 {:life 100} 3 {:life 100} 4 {:life 100}})

(deftest unsafe-locations-test
  (testing "Unsafe locations works"
    (let [locs (unsafe-locations hero-board 1 10 full-life-heroes)
          f    (first locs)
          s    (second locs)]
      (is (contains? f (make-pos 1 5)))
      (is (contains? f (make-pos 2 5)))
      (is (contains? f (make-pos 2 6)))
      (is (not (contains? f (make-pos 3 6))))
      (is (not (contains? s (make-pos 1 3))))
      (is (contains? s (make-pos 3 6))))))

(deftest safe-path-test
  (testing "Same spot test"
    (let [results   (safe-path hero-board (make-pos 1 1) (make-pos 1 1) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :stay))
      (is (= distance 0))))
  (testing "One step test"
    (let [results   (safe-path simple-board (make-pos 1 1) (make-pos 1 2) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 1))))
  (testing "Two step test"
    (let [results   (safe-path hero-board (make-pos 1 1) (make-pos 1 3) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 2))))
  (testing "Around the corner"
    (let [results   (safe-path hero-board (make-pos 2 2) (make-pos 5 2) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :west))
      (is (= distance 5))))
  (testing "Path to beer"
    (let [results   (safe-path hero-board (make-pos 5 2) (make-pos 3 3) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :east))
      (is (= distance 3))))
  (testing "Not safe path"
    (let [results   (safe-path hero-board (make-pos 5 2) (make-pos 1 5) 1 10 full-life-heroes)]
      (is (nil? results)))))

(deftest run-path-test
  (testing "Cornered"
    (let [results   (run-path hero-board (make-pos 6 1) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :stay))))
  (testing "Forced south"
    (let [results   (run-path hero-board (make-pos 1 1) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :south))
      (is (= distance 5))))
  (testing "Running"
    (let [results   (run-path hero-board (make-pos 3 1) 1 10 full-life-heroes)
          distance  (:distance results)
          direction (:direction results)]
      (is (= direction :south))
      (is (= distance 3)))))

(ns oc-freddy.bot-test
  (:require [clojure.test :refer :all]
            [oc-freddy.bot :refer :all]
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
(def simple-board-graph (make-graph simple-board))
(def simple-board-path-func (partial simple-path simple-board-graph))

(deftest not-closer-to-beer-test
  (testing "Return true if not closer to the beer than enemy"
    (let [input {:game {:board simple-board}
                 :hero {:pos (make-pos 3 3)}}]
      (is (false? (not-closer-to-beer input simple-board-path-func {:pos (make-pos 1 1)})))
      (is (true? (not-closer-to-beer input simple-board-path-func {:pos (make-pos 3 4)}))))))

(ns oc-freddy.core
  (:gen-class)
  (:use [oc-freddy.bot :only [bot]])
  (:use [slingshot.slingshot :only [try+, throw+]])
  (:use [clojure.set :only (union)])
  (:use [clojure.core.match :only (match)]))

(def neighbor-directions #{:north, :south, :east, :west})

(defrecord Pos [x y])
(defn make-pos [x y] (Pos. x y))

(def direction-deltas
  {:north {:x -1 :y 0}
   :south {:x 1  :y 0}
   :east  {:x 0  :y 1}
   :west  {:x 0  :y -1}})

(defn safe-min-coord [x] (if (< x 0) 0 x))
(defn safe-max-coord [x size] (if (>= x size) (dec size) x))
(defn safe-coord [x size] (safe-max-coord (safe-min-coord x) size))

(defn move [size from direction]
  (let [delta (get direction-deltas direction)]
    (make-pos (safe-coord (+ (:x from) (:x delta)) size)
              (safe-coord (+ (:y from) (:y delta)) size))))

(defn direction-to [f s]
  (if (> (:x f) (:x s)) :north
    (if (< (:x f) (:x s)) :south
      (if (> (:y f) (:y s)) :west
        (if (< (:y f) (:y s)) :east :stay)))))

(defn tile-index [pos size]
  (+ (* (:x pos) size) (:y pos)))

(defn tile-at [board pos]
  (aget (:tiles board) (tile-index pos (:size board))))

(defn can-move-to [board pos]
  (not (= (:tile (tile-at board pos)) :wall)))

(defn stay-in-place [board pos]
  (let [tile (tile-at board pos)]
    (case (:tile tile)
      :wall   true
      :air    false
      :tavern true
      :mine   true
      :hero   true)))

(defn neighbors-of [size pos]
  (filter #(not (= pos %)) (map move (repeat size) (repeat pos) neighbor-directions)))

(defrecord Node [pos score history])
(defn make-node [pos score history] (Node. pos score history))

(defn insert-into
  ([queue] queue)
  ([queue node]
    (if (empty? queue) [node]
      (let [next (first queue)]
        (if (< (:score next) (:score node))
          (lazy-seq (cons next (insert-into (rest queue) node)))
          (cons node queue)))))
  ([queue node & more]
    (reduce insert-into (insert-into queue node) more)))

(defn distance-from-start [node]
  (count (:history node)))

(defn manhattan-distance [from to]
  (+ (Math/abs (- (:x from) (:x to))) (Math/abs (- (:y from) (:y to)))))

(defn first-direction [node]
  (let [history (reverse (:history node))
        f       (first history)
        s       (second history)]
    (if (<= (count history) 1) :stay (direction-to f s))))

(defn with-pos [node pos]
  (make-node (:pos node) (:score node) (cons pos (:history node))))

(defn simple-path
  ([board from to] (simple-path board to [(make-node from 0 [])] #{from} #{}))
  ([board to open open-added closed]
    (if (empty? open) [Integer/MAX_VALUE :stay]
      (let [current        (first open)
            pos            (:pos current)
            score          (:score current)
            before         (:history current)
            valid-neighbor (fn [p] (and (can-move-to board p) (not (contains? open-added p))))]
        (if (= pos to) [(distance-from-start current) (first-direction (with-pos current pos))]
          (let [neighbors (set (filter valid-neighbor (neighbors-of (:size board) pos)))]
            (recur board to
                   (apply insert-into (rest open)
                          (map (fn [p]
                                 (let [new-pos (if (stay-in-place board p) pos p)]
                                   (make-node new-pos
                                              (inc (+ (distance-from-start current) (manhattan-distance new-pos to)))
                                              (cons pos before)))) (shuffle neighbors)))
                   (union neighbors open-added)
                   (conj closed pos))))))))

(defn all-beers [board]
  (map :pos (filter #(= (:tile %) :tavern) (:tiles board))))

(defn capturable-mines [board hero-id]
  (map :pos
    (filter #(and (= (:tile %) :mine) (not (= (:of %) hero-id)))
            (:tiles board))))


(ns oc-freddy.core
  (:use [clojure.set :only (union)])
  (:use [clojure.core.match :only (match)]))

(def neighbor-directions #{:north, :south, :east, :west})

(defrecord Pos [x y])
(defn make-pos
  ([x y] (Pos. x y))
  ([xy] (Pos. (:x xy) (:y xy))))

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

(defn stay-in-place? [board pos]
  (let [tile (tile-at board pos)]
    (case (:tile tile)
      :wall   true
      :air    false
      :tavern true
      :mine   true
      :hero   false)))

(defn neighbors-of [size pos]
  (filter #(not (= pos %)) (map move (repeat size) (repeat pos) neighbor-directions)))

(defrecord Node [pos score history])
(defn make-node [pos score history] (Node. pos score history))

(defn insert-into
  ([queue] queue)
  ([queue node]
    (lazy-seq
      (if (nil? (first queue)) [node]
        (let [next (first queue)]
          (if (< (:score next) (:score node))
            (cons next (insert-into (rest queue) node))
            (cons node queue))))))
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

(defrecord Route [distance direction destination])
(defn make-route
  ([distance-and-direction position] (make-route (first distance-and-direction) (second distance-and-direction) position))
  ([distance direction position] (Route. distance direction position)))

(defn not-visited? [before pos]
  (not (contains? (set before) pos)))

(defn all-paths-search
  ([board from to]
    (lazy-seq (all-paths-search board (make-pos to) [(make-node from 0 [])] (* 2 (:size board)))))
  ([board to open max-distance]
    (if (empty? open) []
      (let [current        (first open)
            pos            (:pos current)
            score          (:score current)
            before         (:history current)
            valid-neighbor (fn [p] (and (not (= p pos)) (can-move-to board p) (not-visited? before p)))]
        (if (= pos to) (cons (make-route (distance-from-start current) (first-direction (with-pos current pos)) pos)
                             (all-paths-search board to (rest open) max-distance))
          (if (= max-distance (inc (distance-from-start current)))
            (recur board to (rest open) max-distance)
            (let [neighbors (set (filter valid-neighbor (neighbors-of (:size board) pos)))]
              (recur board to
                    (apply insert-into (rest open)
                            (map (fn [p]
                                    (let [new-pos (if (and (stay-in-place? board p) (not (= to p))) pos p)]
                                      (make-node new-pos
                                                (inc (+ (distance-from-start current) (manhattan-distance new-pos to)))
                                                (cons pos before)))) (shuffle neighbors)))
                    max-distance))))))))

(defn non-wall-tiles [board]
  (map :pos (filter (comp not (partial = :wall) :tile) (:tiles board))))

(defn all-paths [board]
  (let [ps (non-wall-tiles board)
        cs (for [from ps to ps] (vector from to))]
    (into {} (map vector cs (map (partial all-paths-search board) (map first cs) (map second cs))))))

(defn simple-path [paths from to]
  (first (get paths (vector (make-pos from) (make-pos to)))))

(defn all-beers [board]
  (map :pos (filter #(= (:tile %) :tavern) (:tiles board))))

(defn capturable-mines [board hero-id]
  (map :pos
    (filter #(and (= (:tile %) :mine) (not (= (:of %) hero-id)))
            (:tiles board))))

(defn enemy-locations [board hero-id]
  (map :pos
    (filter #(and (= (:tile %) :hero) (not (= (:id %) hero-id)))
            (:tiles board))))

(defn scary-enemy-locations [board hero-id life heroes]
  (map :pos
    (filter #(and (= (:tile %) :hero)
                  (not (= (:id %) hero-id))
                  (> (:life (get heroes (:id %))) life))
            (:tiles board))))

(defn shortest-distance [route-func from ps]
  (let [ds (map (partial manhattan-distance from) ps)]
    (loop [p-and-ds (sort-by second (map vector ps ds))
           shortest nil]
      (if (empty? p-and-ds) shortest
        (let [[p d] (first p-and-ds)]
          (if (> d (or (:distance shortest) Integer/MAX_VALUE)) shortest
            (recur (rest p-and-ds)
              (let [route    (route-func from p)]
                (if (< (or (:distance route) Integer/MAX_VALUE)
                       (or (:distance shortest) Integer/MAX_VALUE)) route shortest)))))))))

(defn closest-beer [board paths pos]
  (shortest-distance (partial simple-path paths) pos (all-beers board)))

(defn closest-capturable-mine [board paths pos hero-id]
  (shortest-distance (partial simple-path paths) pos (capturable-mines board hero-id)))

(defn capturable-mines? [board hero-id]
  (not (empty? (capturable-mines board hero-id))))

(defn mine-belongs-to-hero? [board pos hero-id]
  (= (:of (tile-at board pos)) hero-id))

(defn unsafe-locations
  ([board hero-id life heroes]
    (let [can-move-from-neighbors (fn [pos] (filter #(not (stay-in-place? board %))
                                                          (neighbors-of (:size board) pos)))
          starting-locations      (scary-enemy-locations board hero-id life heroes)]
      (unsafe-locations can-move-from-neighbors
                        (set (union starting-locations (mapcat can-move-from-neighbors starting-locations))))))
  ([can-move-from-neighbors previous]
    (let [new-set (union previous (set (mapcat can-move-from-neighbors previous)))]
      (lazy-seq (cons new-set (unsafe-locations can-move-from-neighbors new-set))))))

(defn safe-path-search [board to open open-added closed unsafe-seq]
    (if (empty? open) nil ; no path found
      (let [current        (first open)
            pos            (:pos current)
            before         (:history current)
            step           (count before)
            unsafe         (nth unsafe-seq step)
            valid-neighbor (fn [p] (and (not (contains? unsafe pos)) (not (= p pos)) (can-move-to board p) (not (contains? open-added p))))]
        (if (= pos to) (make-route (distance-from-start current) (first-direction (with-pos current pos)) pos)
          (let [neighbors (set (filter valid-neighbor (neighbors-of (:size board) pos)))]
            (recur board to
                   (apply insert-into (rest open)
                          (map (fn [p]
                                 (let [new-pos (if (and (stay-in-place? board p) (not (= to p))) pos p)]
                                   (make-node new-pos
                                              (inc (+ (distance-from-start current) (manhattan-distance new-pos to)))
                                              (cons pos before)))) (shuffle neighbors)))
                   (union neighbors open-added)
                   (conj closed pos)
                   unsafe-seq))))))

(defn safe-path
  ([board from to hero-id life heroes] (safe-path board (unsafe-locations board hero-id life heroes) from to))
  ([board unsafe-seq from to]
    (safe-path-search board (make-pos to) [(make-node from 0 [])] #{from} #{} unsafe-seq)))

(defn closest-safe-beer [board pos unsafe-locations]
  (shortest-distance (partial safe-path board unsafe-locations) pos (all-beers board)))

(defn closest-safe-capturable-mine [board pos hero-id unsafe-locations]
  (shortest-distance (partial safe-path board unsafe-locations) pos (capturable-mines board hero-id)))

(defn all-enemies-and-mines [board hero-id]
  (concat (capturable-mines board hero-id) (enemy-locations board hero-id)))

(defn closest-enemy-or-mine [board pos hero-id]
  (shortest-distance (partial simple-path board) pos (all-enemies-and-mines board hero-id)))

(defn run-path-score [heroes pos]
  (if (empty? heroes) 0
    (- (apply min (map (partial manhattan-distance pos) heroes)))))

(defn run-path-search [board open open-added closed unsafe-seq heroes best]
    (if (empty? open) (make-route (distance-from-start best) (first-direction best) (:pos best))
      (let [current        (first open)
            pos            (:pos current)
            before         (:history current)
            step           (count before)
            unsafe         (nth unsafe-seq step)
            valid-neighbor (fn [p] (and (not (contains? unsafe pos)) (not (= p pos)) (can-move-to board p) (not (contains? open-added p))))]
        (let [neighbors (set (filter valid-neighbor (neighbors-of (:size board) pos)))]
          (recur board
                 (apply insert-into (rest open)
                       (map (fn [p]
                               (let [new-pos (if (stay-in-place? board p) pos p)]
                                 (make-node new-pos
                                           (run-path-score heroes new-pos)
                                           (cons pos before)))) (shuffle neighbors)))
                 (union neighbors open-added)
                 (conj closed pos)
                 unsafe-seq
                 heroes
                 (min-key :score best current))))))

(defn run-path
  ([board from hero-id life heroes] (run-path board from
                                              (unsafe-locations board hero-id life heroes)
                                              (scary-enemy-locations board hero-id life heroes)))
  ([board from unsafe-seq scary-heroes]
    (run-path-search board [(make-node from 0 [])] #{from} #{} unsafe-seq scary-heroes (make-node from 0 []))))


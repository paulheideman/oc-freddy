(ns oc-freddy.core
  (:use [clojure.set :only (union difference)])
  (:use [clojure.core.match :only (match)])
  (:use [loom.graph])
  (:use [loom.alg-generic :only (bf-path)]))

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

(defn can-move-to? [board pos]
  (not (= (:tile (tile-at board pos)) :wall)))

(defn stay-in-place? [board pos]
  (let [tile (tile-at board pos)]
    (case (:tile tile)
      :wall   true
      :air    false
      :tavern true
      :mine   true
      :hero   false)))

(defn can-move-from? [board pos] (not (stay-in-place? board pos)))

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

(defn manhattan-distance-calc [from to]
  (+ (Math/abs (- (:x from) (:x to))) (Math/abs (- (:y from) (:y to)))))
(def manhattan-distance (memoize manhattan-distance-calc))

(defn first-direction [node]
  (let [history (reverse (:history node))
        f       (first history)
        s       (second history)]
    (if (<= (count history) 1) :stay (direction-to f s))))

(defn with-pos [node pos]
  (make-node (:pos node) (:score node) (cons pos (:history node))))

(defn direction-from-path [path]
  (if (< (count (take 2 path)) 2)
    :stay
    (apply direction-to (take 2 path))))

(defrecord Route [distance direction destination])
(defn make-route
  ([distance-and-direction position] (make-route (first distance-and-direction) (second distance-and-direction) position))
  ([distance direction position] (Route. distance direction position))
  ([path] (and path (make-route (count (drop 1 path)) (direction-from-path path) (last path)))))

(defn simple-path [g from to]
  (if (= from to)
    (make-route 0 :stay to)
    (make-route (bf-path (successors g) (make-pos from) (make-pos to)))))

(defn all-beers [board]
  (map :pos (filter #(= (:tile %) :tavern) (:tiles board))))

(defn all-mines [board]
  (map :pos (filter #(= (:tile %) :mine) (:tiles board))))

(defn air? [board pos]
  (= (:tile (tile-at board pos)) :air))

(defn capturable-mines [board hero-id]
  (map :pos
    (filter #(and (= (:tile %) :mine) (not (= (:of %) hero-id)))
            (:tiles board))))

(defn enemy-locations [board hero-id]
  (map :pos
    (filter #(and (= (:tile %) :hero) (not (= (:id %) hero-id)))
            (:tiles board))))

(defn scary-enemy? [path-func pos life enemy]
  (let [distance (or (:distance (path-func pos (:pos enemy))) Integer/MAX_VALUE)]
    (if (= distance 3)
      (> (:life enemy) (- life 20))
      (> (:life enemy) life))))

(defn scary-enemy-locations [board path-func hero-id life hero-pos heroes]
  (map :pos
    (filter #(and (= (:tile %) :hero)
                  (not (= (:id %) hero-id))
                  (scary-enemy? path-func hero-pos life (get heroes (:id %))))
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

(defn closest-beer [board simple-path-func pos]
  (shortest-distance simple-path-func pos (all-beers board)))

(defn closest-capturable-mine
  ([board simple-path-func pos hero-id] (closest-capturable-mine board simple-path-func pos hero-id #{}))
  ([board simple-path-func pos hero-id camped-mines]
    (shortest-distance simple-path-func pos (difference (set (capturable-mines board hero-id)) camped-mines))))

(defn capturable-mines? [board hero-id]
  (not (empty? (capturable-mines board hero-id))))

(defn mine-belongs-to-hero? [board pos hero-id]
  (= (:of (tile-at board pos)) hero-id))

(defn spawn-location-and-life [heroes]
  (into {} (map (fn [h] [(make-pos (:spawnPos h)) (:life h)]) heroes)))

(defn less-one-hit [spawn-life-map]
  (into {} (for [[k v] spawn-life-map] [k (- v 20)])))

(defn not-me? [hero-id h]
  (not (= (:id h) hero-id)))

(defn unsafe-spawn-locations
  ([heroes hero-id] (lazy-seq (unsafe-spawn-locations
                      (less-one-hit (spawn-location-and-life (filter (partial not-me? hero-id) (vals heroes)))))))
  ([spawn-life-map]
    (if (empty? spawn-life-map) []
      (let [dangerous-spawn-locs (for [[s l] (filter #(<= (second %) 20) spawn-life-map)] s)]
        (lazy-seq (cons dangerous-spawn-locs
                  (unsafe-spawn-locations (less-one-hit (apply dissoc spawn-life-map dangerous-spawn-locs)))))))))

(defn unsafe-locations-generator
  ([board path-func hero-id life hero-pos heroes]
    (let [can-move-from-neighbors (fn [pos] (filter #(not (stay-in-place? board %))
                                                          (neighbors-of (:size board) pos)))
          scary-spawn-locations   (unsafe-spawn-locations heroes hero-id)
          starting-locations      (concat (scary-enemy-locations board path-func hero-id life hero-pos heroes)
                                          (first scary-spawn-locations))]
      (unsafe-locations-generator can-move-from-neighbors
                                  (set (union starting-locations (mapcat can-move-from-neighbors starting-locations)))
                                  (rest scary-spawn-locations))))
  ([can-move-from-neighbors previous scary-spawn-locations]
    (let [new-set (union previous (set (mapcat can-move-from-neighbors (concat previous (first scary-spawn-locations)))))]
      (lazy-seq (cons new-set (unsafe-locations-generator can-move-from-neighbors new-set (rest scary-spawn-locations)))))))

(defn unsafe-locations [board path-func hero-id life hero-pos heroes]
  (let [ps (unsafe-locations-generator board path-func hero-id life hero-pos heroes)]
    (concat (take 1 ps) (repeat (nth ps 1)))))

(defn safe-path-search [board to open open-added closed unsafe-seq path-func]
    (if (empty? open) nil ; no path found
      (let [current        (first open)
            pos            (:pos current)
            before         (:history current)
            step           (count before)
            unsafe         (nth unsafe-seq step)
            valid-neighbor (fn [p] (and (not (contains? unsafe pos)) (not (= p pos)) (can-move-to? board p) (not (contains? open-added p))))]
        (if (= pos to) (make-route (distance-from-start current) (first-direction (with-pos current pos)) pos)
          (let [neighbors (set (filter valid-neighbor (neighbors-of (:size board) pos)))]
            (recur board to
                   (apply insert-into (rest open)
                          (map (fn [p]
                                 (let [new-pos (if (and (stay-in-place? board p) (not (= to p))) pos p)]
                                   (make-node new-pos
                                              (inc (+ (distance-from-start current)
                                                      (:distance (path-func new-pos to))))
                                              (cons pos before)))) (shuffle neighbors)))
                   (union neighbors open-added)
                   (conj closed pos)
                   unsafe-seq path-func))))))

(defn safe-path
  ([board path-func from to hero-id life heroes]
    (safe-path board path-func (unsafe-locations board path-func hero-id life from heroes) from to))
  ([board path-func unsafe-seq from to]
    (safe-path-search board (make-pos to) [(make-node from 0 [])] #{from} #{} unsafe-seq path-func)))

(defn closest-safe-beer [board pos unsafe-locations path-func]
  (shortest-distance (partial safe-path board path-func unsafe-locations)
                     pos (all-beers board)))

(defn closest-safe-capturable-mine
  ([board pos hero-id unsafe-locations path-func]
    (closest-safe-capturable-mine board pos hero-id unsafe-locations {} path-func))
  ([board pos hero-id unsafe-locations camped-mines path-func]
    (shortest-distance (partial safe-path board path-func unsafe-locations) pos
                       (difference (set (capturable-mines board hero-id)) camped-mines))))

(defn all-enemies-and-mines [board hero-id]
  (concat (capturable-mines board hero-id) (enemy-locations board hero-id)))

(defn closest-enemy-or-mine [board path-func pos hero-id]
  (shortest-distance path-func pos (all-enemies-and-mines board hero-id)))

(defn closest-beer-or-spawn-area [board path-func spawn-pos pos]
  (shortest-distance path-func pos
                     (concat (filter (partial air? board) (neighbors-of (:size board) spawn-pos))
                             (all-beers board))))

(defn run-direction [board simple-path-func pos scary-pos spawn-pos]
  (let [ps                        (cons pos (filter (partial air? board) (neighbors-of (:size board) pos)))
        far-enough-away?          (fn [d p] (> (or (:distance
                                    (shortest-distance simple-path-func p scary-pos)) Integer/MAX_VALUE) d))
        distance-to-beer-or-spawn (comp :distance (partial closest-beer-or-spawn-area board simple-path-func spawn-pos))
        suitable-ps               (filter (partial far-enough-away? 2) ps)
        semi-suitable-ps          (filter (partial far-enough-away? 1) ps)]
    (if (empty? suitable-ps)
      (if (empty? semi-suitable-ps) :stay
        (direction-to pos (apply min-key distance-to-beer-or-spawn semi-suitable-ps)))
      (direction-to pos (apply min-key distance-to-beer-or-spawn suitable-ps)))))

(defn non-wall? [t]
  (not (= (:tile t) :wall)))

(defn edge-pairs-for-node [board node]
  (map vector (repeat node) (filter (partial can-move-to? board) (neighbors-of (:size board) node))))

(defn edge-pairs [board non-wall-nodes]
  (partition 2
    (flatten (map (partial edge-pairs-for-node board)
                  (filter (partial can-move-from? board) non-wall-nodes)))))

(defn make-graph [board]
  (let [non-wall-tiles (map :pos (filter non-wall? (:tiles board)))]
    (apply add-edges
      (apply add-nodes (digraph) non-wall-tiles)
      (edge-pairs board non-wall-tiles))))

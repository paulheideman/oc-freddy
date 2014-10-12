(ns oc-freddy.bot
  (:use [oc-freddy.core])
  (:use [clojure.set :only (intersection)]))

(defn hero-health [input] (:life (:hero input)))
(defn board [input] (:board (:game input)))
(defn board-size [input] (:size (board input)))
(defn hero-pos [input] (:pos (:hero input)))
(defn hero-spawn-pos [input] (:spawnPos (:hero input)))
(defn hero-id [input] (:id (:hero input)))
(defn hero-life [input] (:life (:hero input)))
(defn hero-gold [input] (:gold (:hero input)))
(defn hero-mine-count [input] (:mineCount (:hero input)))
(defn heroes [input] (into {} (for [[k v] (group-by :id (:heroes (:game input)))] [k (first v)])))
(defn heroes-without-me [input] (dissoc (heroes input) (hero-id input)))

(defn full-health? [input]
  (>= (hero-life input) 99))

(defn money? [input] (> (hero-gold input) 0))

(defn within-spawn-area? [h size]
  (let [pos       (:pos h)
        spawn-pos (:spawnPos h)]
    (or (= pos spawn-pos) (contains? (set (neighbors-of size spawn-pos)) pos))))

(defn make-return [state direction action & ps]
  (prn direction action ps)
  [direction (into (into state [[:action action]])
                   (map (partial apply vector) (partition 2 ps)))])

(defn not-closer-to-beer [input path-func h]
  (> (or (:distance (closest-beer (board input) path-func (:pos h))) Integer/MAX_VALUE)
     (or (:distance (closest-beer (board input) path-func (hero-pos input))) Integer/MAX_VALUE)))

(defn within-one? [board simple-path-func pos h]
  (< (or (:distance (simple-path-func pos (:pos h))) Integer/MAX_VALUE) 3))

(defn not-close-enough-to-beer [input path-func h]
  (> (or (:distance (closest-beer (board input) path-func (:pos h))) Integer/MAX_VALUE) (/ (:life h) 20)))

(defn not-next-to-beer? [input h]
  (empty? (intersection (set (neighbors-of (board-size input) (:pos h))) (set (all-beers (board input))))))

(defn not-my-bot? [input h]
  (not (= (hero-id input) (:id h))))

(defn sparable-enemy [input simple-path-func h]
  (let [sparable-life         (< (:life h) (+ (hero-life input) 20))
        enough-life           (or (> (hero-life input) 20) (<= (hero-life input) 20))
        not-within-spawn-area (not (within-spawn-area? h (board-size input)))
        not-next-to-beer      (or (not-next-to-beer? input h) (<= (:life h) 20))
        distance              (get (simple-path-func (hero-pos input) (:pos h)) :distance Integer/MAX_VALUE)]
    (and (not-my-bot? input h) sparable-life enough-life not-within-spawn-area not-next-to-beer
         (<= distance 2))))

(defn sparable-enemies [input simple-path-func]
  (filter (partial sparable-enemy input simple-path-func) (vals (heroes input))))

(defn spar-enemy [input state]
  (let [targets (sparable-enemies input (:simple-path-func state))
        paths   (map #((:simple-path-func state) (hero-pos input) (:pos %)) targets)
        path    (first paths)]
    (if-not (empty? path)
      (make-return state (:direction path) :spar-enemy :target (:destination path)))))


(defn vulnerable-enemy [input simple-path-func h]
  (and (< (:life h) (- (hero-life input) 20))
       (not (within-spawn-area? h (board-size input)))
       (or (and (within-one? (board input) simple-path-func (hero-pos input) h)
                (not-close-enough-to-beer input simple-path-func h))
           (not-closer-to-beer input simple-path-func h))))

(defn vulnerable-enemies [input simple-path-func]
  (filter (partial vulnerable-enemy input simple-path-func) (vals (heroes input))))

(defn has-mines? [h]
  (> (:mineCount h) 0))

(defn vulnerable-enemies-with-mines [input simple-path-func]
  (filter has-mines? (vulnerable-enemies input simple-path-func)))

(defn kill-enemy [input state]
  (let [targets    (vulnerable-enemies-with-mines input (:simple-path-func state))
        paths      (map #(safe-path (board input) (hero-pos input) (:pos %) (hero-id input)
                                    (- (hero-life input) 20) (heroes input)) targets)
        path       (first (sort-by :distance (filter (comp not nil?) paths)))]
    (if-not (empty? path)
      (if (<= (:distance path) (:size (board input)))
        (make-return state (:direction path) :kill-enemy :target (:destination path))))))

(defn tavern-neighbors [input]
  (map :pos (filter #(= (:tile %) :tavern)
                    (map (partial tile-at (board input)) (neighbors-of (board-size input) (hero-pos input))))))

(defn get-full-health [input state]
  (let [beer (tavern-neighbors input)]
    (if (and (money? input) (< (hero-life input) 99) (not (empty? beer)))
      (make-return state
        (first (map (partial direction-to (hero-pos input))
                    (shuffle beer)))
        :get-full-health))))

(defn enemy-can-strike? [pos life simple-path-func h]
  (and (>= (:life h) (- life 20))
       (= (or (:distance (simple-path-func pos (:pos h))) Integer/MAX_VALUE) 2)))

(defn enemies-can-strike? [pos life simple-path-func heroes]
  (not (empty? (filter (partial enemy-can-strike? pos life simple-path-func) (vals heroes)))))

(defn go-to-mine [input unsafe-locations state]
  (let [safe-mine (closest-safe-capturable-mine (board input) (hero-pos input) (hero-id input) unsafe-locations)]
    (if (and (> (- (hero-life input) (or (:distance safe-mine) Integer/MAX_VALUE)) 20)
             (not (enemies-can-strike? (hero-pos input) (hero-life input)
                                       (:simple-path-func state) (heroes-without-me input))))
      (and safe-mine
        (make-return state (:direction safe-mine) :go-to-mine :destination (:destination safe-mine))))))

(defn go-to-beer [input unsafe-locations state]
  (if (and (money? input) (not (full-health? input)))
    (let [safe-beer (closest-safe-beer (board input) (hero-pos input) unsafe-locations)]
      (and safe-beer (make-return state (:direction safe-beer) :go-to-beer :destination (:destination safe-beer))))))

(defn suicide [input state]
  (if (zero? (hero-mine-count input))
    (make-return state (:direction (closest-enemy-or-mine (board input) (:simple-path-func state)
                                                          (hero-pos input) (hero-id input)))
                       :suicide)))

(defn run [input scary-enemies state]
    (make-return state
                 (run-direction (board input) (:simple-path-func state)
                                (hero-pos input) scary-enemies (hero-spawn-pos input))
                 :run))

(defn all-same? [p & ps]
  (every? (partial = p) ps))

(defn break-loop [state]
  (let [previous-locations (:previous-locations state)]
    (if (> (count previous-locations) 10)
      (loop [i 1]
        (if (<= i 5)
          (if (apply all-same? (take (/ 10 i) (partition i previous-locations)))
            (make-return state (first (shuffle neighbor-directions)) :break-loop)
            (recur (inc i))))))))

(defn bot [input state]
  (let [unsafe-locations   (unsafe-locations (board input) (hero-id input) (hero-life input) (heroes input))
        scary-enemies      (scary-enemy-locations (board input) (hero-id input) (hero-life input) (heroes input))
        graph              (get state :graph (make-graph (board input)))
        simple-path-func   (memoize (fn [from to] (simple-path graph from to)))
        previous-locations (cons (hero-pos input) (get state :previous-locations []))
        next-state         {:graph              graph
                            :simple-path-func   simple-path-func
                            :previous-locations previous-locations}]
    (or (break-loop next-state)
        (spar-enemy input next-state)
        (kill-enemy input next-state)
        (get-full-health input next-state)
        (go-to-mine input unsafe-locations next-state)
        (go-to-beer input unsafe-locations next-state)
        (suicide input next-state)
        (run input scary-enemies next-state))))

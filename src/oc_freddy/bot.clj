(ns oc-freddy.bot
  (:use [oc-freddy.core])
  (:use [clojure.set :only (intersection)]))

(defn hero-health [input] (:life (:hero input)))
(defn board [input] (:board (:game input)))
(defn board-size [input] (:size (board input)))
(defn hero-pos [input] (:pos (:hero input)))
(defn hero-spawn-pos [input] (make-pos (:spawnPos (:hero input))))
(defn hero-id [input] (:id (:hero input)))
(defn hero-life [input] (:life (:hero input)))
(defn hero-gold [input] (:gold (:hero input)))
(defn hero-mine-count [input] (:mineCount (:hero input)))
(defn heroes [input] (into {} (for [[k v] (group-by :id (:heroes (:game input)))] [k (first v)])))
(defn heroes-without-me [input] (dissoc (heroes input) (hero-id input)))

(defn full-health? [input]
  (>= (hero-life input) 80))

(defn money? [input] (> (hero-gold input) 0))

(defn within-spawn-area? [h size]
  (let [pos       (:pos h)
        spawn-pos (:spawnPos h)]
    (or (= pos spawn-pos) (contains? (set (neighbors-of size spawn-pos)) pos))))

(defn make-return [state direction action & ps]
  (print direction action ps)
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
        paths      (map #(safe-path (board input) (:simple-path-func state) (hero-pos input) (:pos %) (hero-id input)
                                    (- (hero-life input) 20) (heroes input)) targets)
        path       (first (sort-by :distance (filter (comp not nil?) paths)))]
    (if-not (empty? path)
      (if (< (hero-life input) 80)
        (if (<= (:distance path) (Math/sqrt (:size (board input))))
          (make-return state (:direction path) :kill-enemy
                       :target (:destination path) :criteria :easy-kill))
        (if (<= (:distance path) (:size (board input)))
          (make-return state (:direction path) :kill-enemy 
                       :target (:destination path) :criteria :ready-to-kill))))))

(defn tavern-neighbors [input]
  (map :pos (filter #(= (:tile %) :tavern)
                    (map (partial tile-at (board input)) (neighbors-of (board-size input) (hero-pos input))))))

(defn get-full-health [input state]
  (let [beer (tavern-neighbors input)]
    (if (and (money? input) (<= (hero-life input) 80) (not (empty? beer)))
      (make-return state
        (first (map (partial direction-to (hero-pos input))
                    (shuffle beer)))
        :get-full-health))))

(defn enemy-can-strike? [pos life simple-path-func h]
  (and (>= (:life h) (- life 20))
       (<= (or (:distance (simple-path-func pos (:pos h))) Integer/MAX_VALUE) 2)))

(defn enemies-can-strike? [pos life simple-path-func heroes]
  (not (empty? (filter (partial enemy-can-strike? pos life simple-path-func) (vals heroes)))))

(defn go-to-mine [input unsafe-locations state camped-mines path-func]
  (let [safe-mine (closest-safe-capturable-mine (board input) (hero-pos input)
                                                (hero-id input) unsafe-locations camped-mines path-func)]
    (if (and (> (- (hero-life input) (or (:distance safe-mine) Integer/MAX_VALUE)) 20)
             (not (enemies-can-strike? (hero-pos input) (hero-life input)
                                       (:simple-path-func state) (heroes-without-me input))))
      (and safe-mine
        (make-return state (:direction safe-mine) :go-to-mine :destination (:destination safe-mine))))))

(defn go-to-beer [input unsafe-locations state path-func]
  (if (and (money? input) (not (full-health? input)))
    (let [safe-beer (closest-safe-beer (board input) (hero-pos input) unsafe-locations path-func)]
      (and safe-beer (make-return state (:direction safe-beer) :go-to-beer :destination (:destination safe-beer))))))

(defn next-to-unowned-mine [input state]
  (= (or (:distance (closest-capturable-mine (board input) (:simple-path-func state)
                                             (hero-pos input) (hero-id input))) Integer/MAX_VALUE) 1))

(defn suicide [input state]
  (if (zero? (hero-mine-count input))
    (make-return state (:direction (closest-enemy-or-mine (board input) (:simple-path-func state)
                                                          (hero-pos input) (hero-id input)))
                       :suicide)
    (if (and (<= (hero-life input) 20)
             (enemies-can-strike? (hero-pos input) (hero-life input)
                                  (:simple-path-func state) (heroes-without-me input))
             (next-to-unowned-mine input state))
      (make-return state (:direction (closest-capturable-mine (board input) (:simple-path-func state)
                                                              (hero-pos input) (hero-id input))) :suicide))))

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

(defn next-to-unowned-mine? [input state]
  (= (or (:distance (closest-capturable-mine (board input) (:simple-path-func state)
                                             (hero-pos input) (hero-id input))) Integer/MAX_VALUE) 1))

(defn on-position? [pos h]
  (= (:pos h) pos))

(defn on-spawn-position? [input h]
  (on-position? (hero-spawn-pos input) h))

(defn has-more-mines-than-me? [input h]
  (> (:mineCount h) (hero-mine-count input)))

(defn someone-on-my-spawn? [input state]
  (filter (partial on-spawn-position? input) (vals (heroes input))))

(defn someone-on-my-spawn-with-more-mines-than-me? [input state]
  (not (empty? (filter (partial has-more-mines-than-me? input) (someone-on-my-spawn? input state)))))

(defn telefrag [input state]
  (if (and (<= (hero-life input) 20)
           (next-to-unowned-mine? input state)
           (someone-on-my-spawn-with-more-mines-than-me? input state))
    (make-return state (:direction (closest-capturable-mine (board input) (:simple-path-func state)
                                                            (hero-pos input) (hero-id input))) :telefrag)))

(defn remaining-turns [input]
  (- 300 (/ (:turn (:game input)) 4)))

(defn score-accrued-negative [current-mine-count turns average-mine-distance]
  (max (- current-mine-count
          (/ turns (if (zero? average-mine-distance) Integer/MAX_VALUE average-mine-distance))) 0))

(defn score-accrued-positive [current-mine-count turns average-mine-distance total-mine-count]
  (min (+ current-mine-count
          (/ turns (if (zero? average-mine-distance) Integer/MAX_VALUE average-mine-distance))) total-mine-count))

(defn predicted-end-score [input average-mine-distance simple-path-func turns h]
  (let [current-mine-count       (:mineCount h)
        predicted-end-mine-count (if (= (hero-id input) (:id h))
                                   (score-accrued-negative current-mine-count turns average-mine-distance)
                                   (score-accrued-positive current-mine-count turns average-mine-distance
                                        (count (all-mines (board input)))))
        average-mine-count       (/ (+ current-mine-count predicted-end-mine-count) 2)]
    (double (+ (:gold h) (* turns average-mine-count)))))

(defn predicted-end-scores [input state]
  (into {} (for [[id h] (heroes input)] [id (predicted-end-score input (:distance-between-mines state)
                                                                 (:simple-path-func state)
                                                                 (remaining-turns input) h)])))

(defn predicted-winner? [input state]
  (= (hero-id input) (first (apply max-key val (predicted-end-scores input state)))))

(defn play-safe [input state unsafe-locations path-func]
  (if (predicted-winner? input state)
    (let [safe-beer (closest-safe-beer (board input) (hero-pos input) unsafe-locations path-func)]
      (and safe-beer
        (make-return state
          (if (= (:distance safe-beer) 1)
            (if (< (hero-life input) 50)
              (:direction safe-beer)
              :stay)
            (:direction safe-beer))
          :play-safe)))))

(defn closest-mine-to-mine [board path-func ms m]
  (apply min (map #(or (:distance (shortest-distance path-func % (disj ms m))) Integer/MAX_VALUE) (neighbors-of (:size board) m))))

(defn average-distance-between-mines [board path-func]
  (let [ms (set (all-mines board))]
    (if (empty? ms) Double/MAX_VALUE
      (double (/ (apply + (map (partial closest-mine-to-mine board path-func ms) ms)) (count ms))))))

(defn enemy-next-to? [board pos h]
  (contains? (set (neighbors-of (:size board) pos)) (make-pos (:pos h))))

(defn any-enemies-next-to? [board heroes pos]
  (some identity (map (partial enemy-next-to? board pos) (vals heroes))))

(defn currently-camped-mines [input]
  (filter (partial any-enemies-next-to? (board input) (heroes-without-me input)) (all-mines (board input))))

(defn camped-mine-count [camped-mines input]
  (let [currently-camped (set (currently-camped-mines input))]
    (into {} (map (fn [m] [m (max (min (+ (get camped-mines m 0) (if (contains? currently-camped m) 1 -1)) 3) 0)])
                  (all-mines (board input))))))

(defn bot-calc [input state]
  (let [graph                  (get state :graph (make-graph (board input)))
        simple-path-func       (get state :simple-path-func (memoize (fn [from to] (simple-path graph from to))))
        unsafe-locations       (unsafe-locations (board input) simple-path-func (hero-id input) (hero-life input) (hero-pos input) (heroes input))
        scary-enemies          (scary-enemy-locations (board input) simple-path-func (hero-id input) (hero-life input) (hero-pos input) (heroes input))
        previous-locations     (cons (hero-pos input) (get state :previous-locations []))
        distance-between-mines (get state :distance-between-mines
                                    (average-distance-between-mines (board input) simple-path-func))
        camped-mine-count      (camped-mine-count (get state :camped-mine-count {}) input)
        camped-mines           (map first (filter (fn [[m c]] (= 3 c)) camped-mine-count))
        next-state             {:graph                  graph
                                :simple-path-func       simple-path-func
                                :previous-locations     previous-locations
                                :distance-between-mines distance-between-mines
                                :camped-mine-count      camped-mine-count}]
    (or (play-safe input next-state unsafe-locations simple-path-func)
        (telefrag input next-state)
        (break-loop next-state)
        (spar-enemy input next-state)
        (get-full-health input next-state)
        (go-to-mine input unsafe-locations next-state camped-mines simple-path-func)
        (go-to-beer input unsafe-locations next-state simple-path-func)
        (kill-enemy input next-state)
        (suicide input next-state)
        (run input scary-enemies next-state))))

(defn bot [input state]
  (deref (future (bot-calc input state)) 500 (make-return state :stay :timeout)))

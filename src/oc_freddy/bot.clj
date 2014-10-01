(ns oc-freddy.bot
  (:use [oc-freddy.core]))

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

(defn full-health? [input]
  (>= (hero-life input) 99))

(defn money? [input] (> (hero-gold input) 0))

(defn make-return [state direction action & ps]
  (prn "make-return" direction action)
  [direction (into {:action action} (map (partial apply vector) (partition 2 ps)))])

(defn not-closer-to-beer [input g h]
  (> (or (:distance (closest-beer (board input) g (:pos h))) Integer/MAX_VALUE)
     (or (:distance (closest-beer (board input) g (hero-pos input))) Integer/MAX_VALUE)))

(defn within-one? [board g pos h]
  (< (or (:distance (simple-path g pos (:pos h))) Integer/MAX_VALUE) 3))

(defn not-close-enough-to-beer [input g h]
  (> (or (:distance (closest-beer (board input) g (:pos h))) Integer/MAX_VALUE) (/ (:life h) 20)))

(defn vulnerable-enemy [input g h]
  (and (< (:life h) (- (hero-life input) 20))
       (or (and (within-one? (board input) g (hero-pos input) h) (not-close-enough-to-beer input g h))
           (not-closer-to-beer input g h))))

(defn vulnerable-enemies [input g]
  (filter (partial vulnerable-enemy input g) (vals (heroes input))))

(defn has-mines? [h]
  (> (:mineCount h) 0))

(defn vulnerable-enemies-with-mines [input g]
  (filter has-mines? (vulnerable-enemies input g)))

(defn kill-enemy [input state]
  (let [targets    (vulnerable-enemies-with-mines input (:graph state))
        paths      (map #(safe-path (board input) (hero-pos input) (:pos %) (hero-id input)
                                    (- (hero-life input) 20) (heroes input)) targets)
        path       (first (sort-by :distance (filter (comp not nil?) paths)))]
    (if-not (empty? path)
      (make-return state (:direction path) :kill-enemy :target (:destination path)))))

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

(defn spar-enemy [input state]
  nil)

(defn go-to-mine [input unsafe-locations state]
  (let [safe-mine (closest-safe-capturable-mine (board input) (hero-pos input) (hero-id input) unsafe-locations)]
    (if (> (- (hero-life input) (or (:distance safe-mine) Integer/MAX_VALUE)) 20)
      (and safe-mine
        (make-return state (:direction safe-mine) :go-to-mine :destination (:destination safe-mine))))))

(defn go-to-beer [input unsafe-locations state]
  (if (and (money? input) (not (full-health? input)))
    (let [safe-beer (closest-safe-beer (board input) (hero-pos input) unsafe-locations)]
      (and safe-beer (make-return state (:direction safe-beer) :go-to-beer :destination (:destination safe-beer))))))

(defn go-to-spawn [input unsafe-locations state]
  (let [direction (:direction (safe-path (board input) unsafe-locations (hero-pos input) (hero-spawn-pos input)))]
    (and direction (make-return state direction :go-to-spawn))))

(defn suicide [input state]
  (if (zero? (hero-mine-count input))
    (make-return state (:direction (closest-enemy-or-mine (board input) (:graph state)
                                                          (hero-pos input) (hero-id input)))
                       :suicide)))

(defn run [input unsafe-locations scary-enemies state]
    (make-return state (:direction (run-path (board input) (:graph state) (hero-pos input) unsafe-locations scary-enemies)) :run))

(defn bot [input state]
  (let [unsafe-locations (unsafe-locations (board input) (hero-id input) (hero-life input) (heroes input))
        scary-enemies    (scary-enemy-locations (board input) (hero-id input) (hero-life input) (heroes input))
        state            {:graph (get state :graph (make-graph (board input)))}]
    (or (kill-enemy input state)
        (spar-enemy input state)
        (get-full-health input state)
        (go-to-mine input unsafe-locations state)
        (go-to-beer input unsafe-locations state)
        (go-to-spawn input unsafe-locations state)
        (suicide input state)
        (run input unsafe-locations scary-enemies state))))

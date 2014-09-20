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

(defn make-return [direction action & ps]
  (prn "make-return" direction action)
  [direction (into {:action action} (map (partial apply vector) (partition 2 ps)))])

(defn not-closer-to-beer [input h]
  (> (or (:distance (closest-beer (board input) (:pos h))) Integer/MAX_VALUE)
     (or (:distance (closest-beer (board input) (hero-pos input))) Integer/MAX_VALUE)))

(defn vulnerable-enemy [input h]
  (and (< (:life h) (- (hero-life input) 20))
       (not-closer-to-beer input h)))

(defn vulnerable-enemies [input]
  (filter (partial vulnerable-enemy input) (vals (heroes input))))

(defn kill-enemy [input]
  (let [targets    (vulnerable-enemies input)
        paths      (map #(safe-path (board input) (hero-pos input) (:pos %) (hero-id input)
                                    (- (hero-life input) 20) (heroes input)) targets)
        path       (first (sort-by :distance (filter (comp not nil?) paths)))]
    (if-not (empty? path)
      (make-return (:direction path) :kill-enemy :target (:destination path)))))

(defn tavern-neighbors [input]
  (map :pos (filter #(= (:tile %) :tavern)
                    (map (partial tile-at (board input)) (neighbors-of (board-size input) (hero-pos input))))))

(defn get-full-health [input]
  (let [beer (tavern-neighbors input)]
    (if (and (< (hero-life input) 99) (not (empty? beer)))
      (make-return
        (first (map (partial direction-to (hero-pos input))
                    (shuffle beer)))
        :get-full-health))))

(defn spar-enemy [input]
  nil)

(defn go-to-mine [input unsafe-locations]
  (let [safe-mine (closest-safe-capturable-mine (board input) (hero-pos input) (hero-id input) unsafe-locations)]
    (if (> (- (hero-life input) (or (:distance safe-mine) Integer/MAX_VALUE)) 20)
      (and safe-mine
        (make-return (:direction safe-mine) :go-to-mine :destination (:destination safe-mine))))))

(defn go-to-beer [input unsafe-locations]
  (if (and (money? input) (not (full-health? input)))
    (let [safe-beer (closest-safe-beer (board input) (hero-pos input) unsafe-locations)]
      (and safe-beer (make-return (:direction safe-beer) :go-to-beer :destination (:destination safe-beer))))))

(defn go-to-spawn [input unsafe-locations]
  (let [direction (:direction (safe-path (board input) unsafe-locations (hero-pos input) (hero-spawn-pos input)))]
    (and direction (make-return direction :go-to-spawn))))

(defn suicide [input]
  (if (zero? (hero-mine-count input))
    (make-return (:direction (closest-enemy-or-mine (board input) (hero-pos input) (hero-id input))) :suicide)))

(defn run [input unsafe-locations scary-enemies]
    (make-return (:direction (run-path (board input) (hero-pos input) unsafe-locations scary-enemies)) :run))

(defn bot [input]
  (let [unsafe-locations (unsafe-locations (board input) (hero-id input) (hero-life input) (heroes input))
        scary-enemies    (scary-enemy-locations (board input) (hero-id input) (hero-life input) (heroes input))]
    (or (kill-enemy input)
        (spar-enemy input)
        (get-full-health input)
        (go-to-mine input unsafe-locations)
        (go-to-beer input unsafe-locations)
        (go-to-spawn input unsafe-locations)
        (suicide input)
        (run input unsafe-locations scary-enemies))))

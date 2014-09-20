(ns oc-freddy.bot
  (:use [oc-freddy.core]))

(defn hero-health [input] (:life (:hero input)))
(defn board [input] (:board (:game input)))
(defn board-size [input] (:size (board input)))
(defn hero-pos [input] (:pos (:hero input)))
(defn hero-spawn-pos [input] (:spawnPos (:hero input)))
(defn hero-id [input] (:id (:hero input)))
(defn hero-life [input] (:life (:hero input)))
(defn heroes [input] (into {} (for [[k v] (group-by :id (:heroes (:game input)))] [k (first v)])))

(defn full-health? [input]
  (>= (hero-life input) 99))

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
        path       (first (sort-by first (filter #(not (nil? %)) paths)))]
    (prn "kill-enemy" targets (hero-pos input) (map :pos targets) (hero-id input) (- (hero-life input) 20) paths)
    (if-not (empty? path)
      (make-return (second path) :kill-enemy :target (:destination path)))))

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

(defn go-to-mine [input]
  (let [safe-mine (closest-safe-capturable-mine (board input) (hero-pos input) (hero-id input) (hero-life input) (heroes input))]
    (if (> (- (hero-life input) (or (:distance safe-mine) Integer/MAX_VALUE)) 20)
      (and safe-mine
        (make-return (:direction safe-mine) :go-to-mine :destination (:destination safe-mine))))))

(defn go-to-beer [input]
  (if (not (full-health? input))
    (let [safe-beer (closest-safe-beer (board input) (hero-id input) (hero-pos input) (hero-life input) (heroes input))]
      (and safe-beer (make-return (:direction safe-beer) :go-to-beer :destination (:destination safe-beer))))))

; [board from to hero-id life heroes]
(defn go-to-spawn [input]
  (let [direction (second (safe-path (board input) (hero-pos input) (hero-spawn-pos input) (hero-id input)
                                     (hero-life input) (heroes input)))]
    (and direction (make-return direction :go-to-spawn))))

(defn run [input]
  nil)

(defn bot [input]
  (prn "mine" (go-to-mine input))
  (or (kill-enemy input)
      (spar-enemy input)
      (get-full-health input)
      (go-to-mine input)
      (go-to-beer input)
      (go-to-spawn input)
      (run input)))

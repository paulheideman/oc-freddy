(ns oc-freddy.bot
  (:use [oc-freddy.core]))

(defn hero-health [input] (:life (:hero input)))
(defn board [input] (:board (:game input)))
(defn hero-pos [input] (:pos (:hero input)))
(defn hero-spawn-pos [input] (:spawnPos (:hero input)))
(defn hero-id [input] (:id (:hero input)))
(defn hero-life [input] (:life (:hero input)))
(defn heroes [input] (into {} (for [[k v] (group-by :id (:heroes (:game input)))] [k (first v)])))

(defn death? [input]
  (and (= (hero-health input) 100) (= (hero-pos input) (hero-spawn-pos input))))

(defn switch-to-get-health [input]
  (let [safe-beer (closest-safe-beer (board input) (hero-id input) (hero-pos input) (hero-life input) (heroes input))
        beer      (closest-beer (board input) (hero-pos input))]
    (if (nil? safe-beer)
      (if (nil? beer) {:state :random}
        {:state :full-health :pos (:destination beer)})
      {:state :full-health :pos (:destination safe-beer)})))

(defn switch-to-acquire-mine [input]
  (if (capturable-mines? (board input) (hero-id input))
    (let [safe-mine (closest-safe-capturable-mine (board input) (hero-pos input) (hero-id input) (hero-life input) (heroes input))
          mine      (closest-capturable-mine (board input) (hero-pos input) (hero-id input))]
      (if (nil? safe-mine)
        (if (nil? mine) {:state :random}
          {:state :acquire-mine :pos (:destination mine)})
        {:state :acquire-mine :pos (:destination safe-mine)}))
    (switch-to-get-health input)))

(defn go-to-mine [input state]
  (if (mine-belongs-to-hero? (board input) (:pos state) (hero-id input)) (switch-to-acquire-mine input)
    (let [safe      (safe-path (board input) (hero-pos input) (:pos state) (hero-id input) (hero-life input) (heroes input))
          path      (simple-path (board input) (hero-pos input) (:pos state))
          direction (if (= :stay (second safe)) (second path) (second safe))
          distance  (if (= :stay (second safe)) (first path) (first safe))]
      [direction (if (> distance 1) state (switch-to-get-health input))])))

(defn go-to-health [input state]
  (let [safe      (safe-path (board input) (hero-pos input) (:pos state) (hero-id input) (hero-life input) (heroes input))
        path      (simple-path (board input) (hero-pos input) (:pos state))
        direction (if (= :stay (second safe)) (second path) (second safe))
        distance  (if (= :stay (second safe)) (first path) (first safe))]
    [direction (if (or (> distance 1) (< (hero-health input) 96))
               state ; stay the same
               (switch-to-acquire-mine input))]))

(defn random [input state]
  [(first (shuffle [:north, :south, :east, :west, :stay])) state])

(defn choose-action [input state]
  (case (:state state)
    :random       (random input state)
    :acquire-mine (let [correct-state (switch-to-acquire-mine input)]
                    (if (= correct-state state) (go-to-mine input state)
                      (recur input correct-state)))
    :full-health  (let [correct-state (switch-to-get-health input)]
                    (if (= correct-state state) (go-to-health input state)
                      (recur input correct-state)))
    nil           (recur input (switch-to-acquire-mine input))))

(defn bot [input state]
  (if (death? input)
    (do
      (println "Died!")
      (choose-action input {}))
    (choose-action input state)))

(ns oc-freddy.bot
  (:use [oc-freddy.core]))

(defn hero-health [input] (:life (:hero input)))
(defn board [input] (:board (:game input)))
(defn hero-pos [input] (:pos (:hero input)))
(defn hero-id [input] (:id (:hero input)))

(defn switch-to-get-health [input]
  (let [beer (closest-beer (board input) (hero-pos input))]
    (if (nil? beer) {:state :random}
      {:state :full-health :pos beer})))

(defn switch-to-acquire-mine [input]
  (if (capturable-mines? (board input) (hero-id input))
    (let [mine (closest-capturable-mine (board input) (hero-pos input) (hero-id input))]
      (if (empty? mine) {:state :random}
        {:state :acquire-mine :pos mine}))
    (switch-to-get-health input)))

(defn go-to-mine [input state]
  (if (mine-belongs-to-hero? (board input) (:pos state) (hero-id input)) (switch-to-acquire-mine input)
    (let [path      (simple-path (board input) (hero-pos input) (:pos state))
          direction (second path)
          distance  (first path)]
      [direction (if (> distance 1) state (switch-to-get-health input))])))

(defn go-to-health [input state]
  (let [path      (simple-path (board input) (hero-pos input) (:pos state))
        direction (second path)
        distance  (first path)]
    [direction (if (or (> distance 1) (< (hero-health input) 96))
               state ; stay the same
               (switch-to-acquire-mine input))]))

(defn random [input state]
  [(first (shuffle [:north, :south, :east, :west, :stay])) state])

(defn choose-action [input state]
  (case (:state state)
    :random       (random input state)
    :acquire-mine (go-to-mine input state)
    :full-health  (go-to-health input state)
    nil           (recur input (switch-to-acquire-mine input))))

(defn bot [input state] (choose-action input state))

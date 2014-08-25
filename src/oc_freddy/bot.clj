(ns oc-freddy.bot
  (:use [oc-freddy.core]))

(defn switch-to-acquire-mine [input]
  (let [mine (closest-capturable-mine (:board (:game input)) (:pos (:hero input)) (:id (:hero input)))]
    (prn "mine" mine "---")
    (if (empty? mine) {:state :random}
      {:state :acquire-mine :pos mine})))

(defn switch-to-get-health [input]
  (let [beer (closest-beer (:game (:board input)))]
    (if (nil? beer) {:state :random}
      {:state :full-health :pos beer})))

(defn go-to-mine [input state]
  [(simple-path-direction (:board (:game input)) (:pos (:hero input)) (:pos state)) state])

(defn go-to-health [input state]
  [(simple-path-direction (:board (:game input)) (:pos (:hero input)) (:pos state)) state])

(defn random [input state]
  [(first (shuffle [:north, :south, :east, :west, :stay])) state])

(defn choose-action [input state]
  (case (:state state)
    :random       (random input state)
    :acquire-mine (go-to-mine input state)
    :full-health  (go-to-health input state)
    nil           (recur input (switch-to-acquire-mine input))))

(defn bot [input state] (choose-action input state))

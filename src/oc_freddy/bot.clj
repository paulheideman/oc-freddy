(ns oc-freddy.bot)

(defn bot [input]
  "Implement this function to create your bot!"
  (first (shuffle ["north", "south", "east", "west", "stay"])))


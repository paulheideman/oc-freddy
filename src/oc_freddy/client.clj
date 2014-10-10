(ns oc-freddy.client
  (:gen-class)
  (:use [oc-freddy.bot :only [bot]])
  (:use [oc-freddy.core :only [make-pos]])
  (:use [slingshot.slingshot :only [try+, throw+]])
  (:use [clojure.set :only (union)])
  (:use [clojure.java.browse :only (browse-url)])
  (:use [clojure.core.match :only (match)]))

(require '[clj-http.client :as http])

(def server-url "http://vindinium.org")

(defn parse-tile [tile]
  (match (vec tile)
        [\space \space] {:tile :air}
        [\# \#] {:tile :wall}
        [\[ \]] {:tile :tavern}
        [\$ \-] {:tile :mine}
        [\$ i] {:tile :mine :of (read-string (str i))}
        [\@ i] {:tile :hero :id (read-string (str i))}))

(defn with-positions [tiles]
  (let [size (int (Math/sqrt (count tiles)))]
    (map (fn [tile x y] (assoc tile :pos (make-pos x y)))
      tiles
      (flatten (map #(repeat size %) (range size)))
      (cycle (range size)))))

(defn parse-tiles [tiles] (to-array (with-positions (map parse-tile (partition 2 (seq tiles))))))

(defn parse-input [input]
  ;(prn input)
  (update-in input [:game :board :tiles] parse-tiles))

(defn request [url, params]
  "makes a POST request and returns a parsed input"
  (try+
    (parse-input (:body (http/post url {:form-params params :as :json})))
    (catch map? {:keys [status body]}
      (println (str "[" status "] " body))
      (throw+))))

(def dir-map
  {:north "north"
   :south "south"
   :east  "east"
   :west  "west"
   :stay  "stay"})

(defn step [from]
  (loop [input from
         state {}]
    (print "(" (:turn (:game input)) "/" (:maxTurns (:game input)) "-" (:life (:hero input)) ")  - ")
    (let [result    (bot input state)
          dir       (first result)
          new-state (second result)
          next      (request (:playUrl input) {:dir (get dir-map dir "stay")})]
      (if (:finished (:game next)) (println "") (recur next new-state)))))

(defn training [secret-key turns]
  (let [input (request (str server-url "/api/training") {:key secret-key :turns turns})]
    (println (str "Starting training game " (:viewUrl input)))
    (browse-url (str (:viewUrl input) "?speed=max"))
    (step input)
    (println (str "Finished training game " (:viewUrl input)))))

(defn arena [secret-key games]
  (loop [it 1]
    (let [p #(println (str "[" it "/" games "] " %))
          _ (p "Waiting for pairing...")
          input (request (str server-url "/api/arena") {:key secret-key})]
      (p (str "Starting arena game " (:viewUrl input)))
      (browse-url (str (:viewUrl input) "?speed=max"))
      (step input)
      (p (str "Finished arena game " (:viewUrl input)))
      (when (< it (read-string games)) (recur (+ it 1))))))

(def usage
  "Usage:
   training <secret-key> <number-of-turns>
   arena <secret-key> <number-of-games")

(defn -main [& args]
  (match (vec args)
         ["training", secret-key, nb] (training secret-key nb)
         ["arena", secret-key, nb] (arena secret-key nb)
         :else (println usage)))

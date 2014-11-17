(ns game2048.sys
  ( :require [lonocloud.synthread :as ->]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random Number Generator Component

(defprotocol RNG
  (^:updater seed- [_ value] "Seed the random number generator.")
  (^:updater gen- [_] "Generate a new number and throw away the old number.")
  (num- [_] "Look at the current generated number."))

(defprotocol Reader
  (^:updater read- [_])
  (value- [_]))

(defprotocol Writer
  (^:updater write- [_ msg]))

(defn rnd-nth
  "Pick a random value from coll and return a new rng plus that
  value. If coll is empty, return the unchanged rng plus nil."
  [rng coll]
  (if (empty? coll)
    nil
    (nth coll (int (* (num- rng) (count coll))))))

(defn weighted-rnd-nth
  [rng pairs]
  (if (empty? pairs)
    nil
    (let [total (apply + (map second pairs))
          target (int (* (num- rng) total))]
      (reduce (fn [running-total [x weight]]
                (let [new-weight (+ running-total weight)]
                  (if (> new-weight target)
                    (reduced x)
                    new-weight))) 0 pairs))))

(defrecord JavaRNG [num obj]
  RNG
  (seed- [self v]
    (.setSeed obj v)
    (assoc self :num v))
  (num- [_] num)
  (gen- [self] (assoc self :num (.nextFloat obj))))

(defn new-rng
  "Return a new random number generator."
  ([] (->JavaRNG nil (java.util.Random.)))
  ([seed] (->JavaRNG seed (java.util.Random. seed))))

(extend-protocol Reader
  java.lang.String
  (read- [_] (read-line))
  (value- [self] self)

  clojure.lang.PersistentList
  (read- [v] (pop v))
  (value- [v] (peek v)))

(extend-protocol Writer
  nil
  (write- [_ msg]
    (println msg)
    _))

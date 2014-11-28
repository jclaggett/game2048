(ns game2048.sys
  (:require #+clj [lonocloud.synthread :as ->])
  #+cljs (:require-macros [lonocloud.synthread :as ->]))

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

#+clj
(defrecord JavaRNG [num obj]
  RNG
  (seed- [self v]
    (.setSeed obj v)
    (assoc self :num v))
  (num- [_] num)
  (gen- [self] (assoc self :num (.nextFloat obj))))

#+cljs
(extend-protocol RNG
  nil
  (seed- [self v] (throw (js/Error. "Seeded rng unsupported in the browser")))
  (num- [_] (Math/random))
  (gen- [self] self))

(defn new-rng
  "Return a new random number generator."
  ([] #+clj (->JavaRNG nil (java.util.Random.)))
  ([seed]
     #+clj (->JavaRNG seed (java.util.Random. seed))
     #+cljs (throw (js/Error. "Seeded rng unsupported in the browser"))))

(extend-protocol Reader
  #+clj java.lang.String #+cljs js/String
  (read- [_] #+clj (read-line) #+cljs "unsupported")
  (value- [self] self)

  #+clj clojure.lang.PersistentList #+cljs cljs.core.List
  (read- [v] (pop v))
  (value- [v] (peek v)))

(extend-protocol Writer
  nil
  (write- [_ msg]
    (println msg)
    _))

#+cljs (set! *print-fn* (fn [& args] (.log js/console (apply str args))))

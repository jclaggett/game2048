(ns game2048.player
  (:require [game2048.sys :as sys]
            [game2048.core :as core :refer [up down left right]]
            [lonocloud.synthread :as ->]))

(defrecord PlayerReadWrite [reader writer]
  core/Player
  (make-move- [self board-over]
    (->/do self
      (->/assoc :writer (sys/write- board-over)
                :reader (->/when-not (:over board-over)
                          sys/read-))))
  (get-move- [self]
    (-> reader sys/value-)))

(defrecord PlayerCorner [cmd]
  core/Player
  (make-move- [self {:keys [board over]}]
    (->/do self
      (->/if over
        (->/assoc
         :cmd (->/reset :quit) ;; not really needed
         :writer (sys/write- {:board board}))
        (->/do
          (assoc :cmd
            (cond
             (not= board (core/tilt board down)) :down
             (not= board (core/tilt board left)) :left
             (not= board (core/tilt board right)) :right
             (not= board (core/tilt board up)) :up
             :true :quit))))))
  (get-move- [self] cmd))

(def inside-cells #{5 6 9 10})
(def edge-cells #{1 2 4 7 8 11 13 14})
(def corner-cells #{0 3 12 15})
(def outside-cells (into edge-cells corner-cells))

(def ^:static weight-array
  (long-array [4 3 2 1, 5 6 7 8, 12 11 10 9, 13 14 15 16]))

(defn score-leaf ^long [board]
  (loop [i (int 0), ttl 0]
    (if (> i 15)
      ttl
      (let [tile ^long (nth board i)]
        (recur (inc i)
               (+ ttl (* (aget (longs weight-array) i) tile tile)))))))

(def ^:static cmd-array
  (to-array [down left right up]))

(def ^:static cmd-key-array
  (to-array [:down :left :right :up]))

(defn score [in-board ^long remain-depth]
  (if (< remain-depth 1)
    [:? (score-leaf in-board)]

    (loop [best-cmd -1
           best-score -1
           cmd-num 0]
      (if (> cmd-num 3)
        [(aget cmd-key-array best-cmd) best-score]
        (let [board (core/tilt in-board (aget cmd-array cmd-num))
              blanks (take 5 (shuffle (core/find-blanks board)))
              score (if (= board in-board)
                      0
                      (if (empty? blanks)
                        (score-leaf board)
                        (loop [[blank & blanks] blanks
                               cnt 0
                               ttl 0]
                          (if-not blank
                            (if (zero? cnt)
                              0
                              (/ (double ttl) (double cnt)))
                            (if (contains? inside-cells blanks)
                              (recur blanks cnt ttl)
                              (recur blanks (inc cnt) (+ ttl (long (second (score (assoc board blank 2)
                                                                                  (dec remain-depth)))))))))))]
          (if (>= score best-score)
            (recur cmd-num score (inc cmd-num))
            (recur best-cmd best-score (inc cmd-num))))))))

(defrecord PlayerSearch [cmd]
  core/Player
  (make-move- [self {:keys [board over]}]
    (->/do self
           (->/assoc :writer (sys/write- (core/board-str board over)))
           (->/let [[cmd score] (score board 3)]
             (->/if (= board (core/tilt board (core/cmd-map cmd)))
               (assoc :cmd :quit)
               (assoc :cmd cmd)))))
  (get-move- [self] cmd))

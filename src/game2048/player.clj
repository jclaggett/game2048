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

(defn score-leaf [board]
  (reduce +
          (map (fn [tile weight]
                 (* weight tile tile))
               board
               [4 3 2 1, 5 6 7 8, 12 11 10 9, 13 14 15 16])))

(defn score [in-board remain-depth]
  (if (< remain-depth 1)
    [:? (score-leaf in-board)]
    (reduce (fn [[best-cmd best-score :as old-best] cmd]
              (let [board (core/tilt in-board (core/cmd-map cmd))
                    blanks (core/find-blanks board)
                    new-depth (dec remain-depth)
                    score (if (= board in-board)
                            0
                            (if (empty? blanks)
                              (score-leaf board)
                              (let [blanks (remove inside-cells blanks)]
                                (/ (reduce (fn [ttl idx]
                                             (+ ttl (second (score (assoc board idx 2)
                                                                   new-depth))))
                                           0 blanks)
                                   (double (count blanks))))))]
                (if (>= score best-score)
                  [cmd score]
                  old-best)))
            [:? 0]
            [:down :left :right :up])))

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

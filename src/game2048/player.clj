(ns game2048.player
  (:require [game2048.sys :as sys]
            [game2048.core :as core]
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
             (not= board (core/tilt board core/down)) :down
             (not= board (core/tilt board core/left)) :left
             (not= board (core/tilt board core/right)) :right
             (not= board (core/tilt board core/up)) :up
             :true :quit))))))
  (get-move- [self] cmd))
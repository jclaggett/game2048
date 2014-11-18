(ns game2048.player
  (:require [game2048.sys :as sys]
            [game2048.core :as core]
            [lonocloud.synthread :as ->]))

(defrecord Writer2048 [writer]
  sys/Writer
  (write- [self game]
    (update-in self [:writer] sys/write-
               (core/board-str (:board game) (:over game)))))

(defrecord WriterCounter [i]
  sys/Writer
  (write- [self game]
    (-> self
        (->/if (:over game)
               (->/assoc :writer (sys/write-
                                   (str (core/board-str (:board game) false)
                                        "\nBoard iterations: " i)))
               (->/assoc :i inc)))))

(defrecord PlayerReadWrite [reader writer]
  core/Player
  (make-move- [self board-over]
    (->/do self
      (->/assoc :writer (sys/write- board-over)
                :reader (->/when-not (:over board-over)
                          sys/read-))))
  (get-move- [self]
    (-> reader sys/value-)))

(defn choose-dir
  "Pick the first direction that changes the board or quit otherwise."
  [board dirs]
  (reduce (fn [dir next-dir]
            (if (not= board (core/tilt board (core/cmd-map dir)))
              (reduced dir)
              next-dir))
          (conj dirs :quit)))

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
            (choose-dir board [:down :left :right :up]))))))
  (get-move- [self] cmd))

;; Goals
;; collapse column 0 up
;; occupy column 0
;; collapse column 1 down
;; occupy column 1
;; collapse column 2 up
;; occupy column 2

(defrecord Serpent [cmd log]
  core/Player
  (make-move- [<> {:keys [board over]}]
    (->/do <>
           (assoc :cmd (choose-dir board (if (= 0 (board 0))
                                           [:up :left]
                                           [:left :up :down :right])))
           (->/when (= :quit (:cmd <>))
             (update-in [:log] sys/write- {:board board :over false}))))
  (get-move- [self] cmd))

(def serpent (->Serpent :quit (->Writer2048 nil)))

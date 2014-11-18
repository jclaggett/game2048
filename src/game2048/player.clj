(ns game2048.player
  (:require [game2048.sys :as sys]
            [game2048.core :as core]
            [lonocloud.synthread :as ->]))

(defrecord Reader2048 [text]
  sys/Reader
  (read- [self]
    (update-in self [:text] sys/read-))
  (value- [_]
    ({"h" :left
      "j" :down
      "k" :up
      "l" :right
      "q" :quit} text)))

(defrecord ReaderRandom [rng]
  sys/Reader
  (read- [self]
    (update-in self [:rng] sys/gen-))
  (value- [_]
    (sys/weighted-rnd-nth rng [[:up 1] [:left 100] [:down 1000] [:right 100]])))

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

(defn choose-goal [board]
  (cond
    (not= (core/col board 0) (core/col (core/tilt board core/up) 0)) [:up]
    (or (not= (core/col board 0) (core/col (core/tilt board core/left) 0))
        (some zero? (core/col board 0))) [:left :up :down :right]

    (not= (core/col board 1) (core/col (core/tilt board core/down) 1)) [:down]
    (or (not= (core/col board 1) (core/col (core/tilt board core/left) 1))
        (some zero? (core/col board 1))) [:left :down :up :right]

    (not= (core/col board 2) (core/col (core/tilt board core/up) 2)) [:up]
    (or (not= (core/col board 2) (core/col (core/tilt board core/left) 2))
        (some zero? (core/col board 2))) [:left :up :down :right]

    :default [:down :left :up :right]
    )
  )

(defrecord Serpent [cmd log]
  core/Player
  (make-move- [<> {:keys [board over]}]
    (->/do <>
           (assoc :cmd (choose-dir board
                                   (choose-goal board)
                                   #_(if (= 0 (board 0))
                                           [:up :left]
                                           [:left :up :down :right])))
           (->/when true #_(= :quit (:cmd <>))
             (update-in [:log] sys/write- {:board board :over false})
             )))
  (get-move- [self] cmd))

(def serpent (->Serpent :quit (->Writer2048 nil)))

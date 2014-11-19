(ns game2048.player
  (:require [game2048.sys :as sys]
            [game2048.core :as core :refer [up down left right]]
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

;; PlayerSearch
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

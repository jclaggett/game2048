(ns game2048.main
  (:require [game2048.sys :as sys]
            [game2048.player :as player]
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

(defn new-game
  "Return a new game complete with initial pollution."
  [& {:keys [seed player reader writer]}]
  (->/do (core/->Game core/empty-board
                      (if seed
                        (sys/new-rng seed)
                        (sys/new-rng))
                      (or player (player/->PlayerReadWrite
                                  (or reader (->Reader2048 ""))
                                  (or writer (player/->Writer2048 nil)))))
         core/pollute
         core/pollute))

(comment

  ;; interactive
  (-> (new-game) core/play-game)

  ;; preset moves
  (-> (new-game :seed 0 :reader '(:skip :down :down :down :quit)) core/play-game)

  ;; weighted random player, counting the moves in the writer
  (-> (new-game :reader (->ReaderRandom (sys/new-rng)) :writer (->WriterCounter 0))
      core/play-game)

  ;; determanistic player
  (-> (new-game :player (player/->PlayerCorner :dmy)) core/play-game))

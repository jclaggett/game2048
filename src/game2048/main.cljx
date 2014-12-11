(ns game2048.main
  (:require [game2048.sys :as sys]
            [game2048.player :as player]
            [game2048.core :as core]
            #+clj [lonocloud.synthread :as ->]
            #+cljs [game2048.ui :as ui])
  #+cljs (:require-macros [lonocloud.synthread :as ->]))

(defn new-game
  "Return a new game complete with initial pollution."
  [& {:keys [seed player reader writer observer]}]
  (->/do (core/->Game core/empty-board
                      (if seed
                        (sys/new-rng seed)
                        (sys/new-rng))
                      (or player (player/->PlayerSearch :dmy))
                      (or observer
                          #+clj (player/->ObserverStdout nil)
                          #+cljs (ui/make-ui)))
         core/pollute
         core/pollute
         (update-in [:observer] core/observe- (:board <>) (:over <>) :pollute)))

(comment

  ;; interactive
  (-> (new-game) core/play-game)

  ;; preset moves
  (-> (new-game :seed 0 :reader '(:skip :down :down :down :quit)) core/play-game)

  ;; weighted random player, counting the moves in the writer
  (-> (new-game :reader (->ReaderRandom (sys/new-rng)) :writer (->WriterCounter 0))
      core/play-game)

  ;; simple determanistic player
  (-> (new-game :player (player/->PlayerCorner :dmy)) core/play-game)

  ;; smarter player
  (-> (new-game :player (player/->PlayerSearch :dmy)) core/play-game)
  )

(defn go []
  (-> (new-game :player (player/->PlayerSearch :dmy)) core/play-game))

#+cljs (go)

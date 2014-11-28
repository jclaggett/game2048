(ns game2048.player1
    (:require [game2048.core :as g]))

;; So, to make a player, I need to make a custom game which takes control of
;; the both the reader and writer This is a problem because the current reader
;; and writer take and receive strings instead of nice Clojure data. My first
;; step is to make better readers and writers.


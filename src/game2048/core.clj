(ns game2048.core
  (:require [game2048.sys :as sys]
            [lonocloud.synthread :as ->]))

(comment
  "I'd like to define a 2048 component.
  Steps towards that goal:
    1. defrecord with all the state needed by this component including
       any subcomponents.
    2. Use only functional subcomponents: all functions must act as
       updaters or getters.
    3. Mark state that needs to be immutable.")

(def empty-board
  [0,0,0,0
   0,0,0,0
   0,0,0,0
   0,0,0,0])

;; TODO These functions all hard code a 4x4 board maybe it would be better to
;; generate these functions based on the board dimensions.
(defn left
  "Return index left of idx or nil when at the edge."
  [idx]
  (when (not= 0 (rem idx 4))
    (- idx 1)))

(defn up
  "Return index above idx or nil when at edge."
  [idx]
  (when (>= idx 4)
    (- idx 4)))

(defn right
  "Return index right of idx or nil when at edge."
  [idx]
  (when (not= 3 (rem idx 4))
    (+ idx 1)))

(defn down
  "Return index below idx or nil when at edge."
   [idx]
   (when (< idx 12)
    (+ idx 4)))

(defn cells
  "Returns a sequence of board indicies specified by direction. Each specific
  sequence is designed so that values in the cells are only operated once by
  slide or combine operations."
  [direction]
  (let [idxs (range 16)
        size 4]
    (condp = direction
      left (map #(+ (int (/ % size)) (* size (rem % size)))
                idxs)
      right (reverse (cells left))
      down (reverse (cells up))
      up idxs)))

(defn find-blank
  "Return the index of the farthest blank cell in direction, starting at idx and
  with only blank cells between them. If on the edge or next to an existing
  cell, return idx."
  [board direction idx]
  (loop [cell idx]
    (let [new-cell (direction cell)]
      (if (zero? (get board new-cell -1))
        (recur new-cell)
        cell))))

(defn slide
  "Slide each non-blank in direction until stopped by another value or
  the edge."
  [board direction]
  (reduce
    (fn [board cell]
      (if (zero? (get board cell))
        board
        (let [new-cell (find-blank board direction cell)]
          (if (not= cell new-cell)
            (assoc board
                   new-cell (get board cell)
                   cell 0)
            board))))
    board
    (cells direction)))

(defn combine
  "Combine equal values that are adjacent in the direction specified. Replace
  the 'first' value in that direction with the sum of both values."
  [board direction]
  (reduce
    (fn [board cell]
      (if (= (get board cell)
             (get board (direction cell)))
        (assoc board
               (direction cell) (* 2 (get board cell))
               cell 0)
        board))
    board
    (cells direction)))

(defn tilt [board direction]
  "Tilt the board according to the specified direction."
  (-> board
      (slide direction)
      (combine direction)
      (slide direction)))

(defn find-blanks
  "Return a sequence of indicies for all blank cells found in board."
  [board]
  (->> board
       (map list (range))
       (filter #(zero? (second %)))
       (map first)))

(defn rnd-val
  "Randomly pick a new value. 1/10 = 4, 9/10 = 2."
  [rng]
  (if (< (sys/num- rng) 0.1)
    4
    2))

(defn pollute
  "Add a random value to a random blank cell."
  [game blanks]

  ;; with syntax support and updater functions only
  (-> game
      (->/let [idx (by :rng sys/gen- (sys/rnd-nth blanks))
               val (by :rng sys/gen- rnd-val)]
        (assoc-in [:board idx] val))))

(defn show
  "Print current board state."
  [{:keys [board over] :as game}]
  (-> game
      (update-in [:writer] sys/write-
                 (if over
                   "Game over!"
                   (->> board
                        (map #(if (zero? %) '. %))
                        (partition 4)
                        (map #(str (vec %) "\n"))
                        (cons "_________\n")
                        (apply str)
                        )))))

;; Rubber meets the road from this point on.
;; Everything above here was
(defrecord Game [^:immutable board, rng, writer, reader])

(defn parse-input
  "Parse the input text to determine which direction to tilt the board."
  [text]
  (condp = text
    "h" left
    "j" down
    "k" up
    "l" right
    "q" :quit
    nil))

(defn play-turn
  "Play a single turn."
  [game]
  (->/do game
         (->/let [cmd (by :reader sys/read- (-> sys/value- parse-input))]
           (->/when-not (nil? cmd)
             (->/if (= :quit cmd)
               (assoc :over true)
               (->/let [old-board (:board <>)
                        new-board (by :board (tilt cmd))
                        blanks (find-blanks new-board)]
                 (->/if (= old-board new-board)
                   (->/when (= old-board
                               (tilt old-board up)
                               (tilt old-board left))
                     (assoc :over true))
                   (pollute blanks))))
             show))))

(defn new-game
  "Return a new game complete with initial pollution."
  [& {:keys [seed reader]}]
  (->/do (->Game empty-board
                 (if seed
                   (sys/new-rng seed)
                   (sys/new-rng))
                 nil
                 (or reader ""))
         (pollute (-> <> :board find-blanks))
         (pollute (-> <> :board find-blanks))
         show))

(defn play-game
  "Play an entire game."
  [game]
  (loop [game game]
    (if (:over game)
      game
      (recur (play-turn game)))))

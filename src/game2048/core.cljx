(ns game2048.core
  (:require [game2048.sys :as sys]
            #+clj [lonocloud.synthread :as ->]
            #+cljs [goog.string.format])
  #+cljs (:require-macros [lonocloud.synthread :as ->]))

#+cljs (def format goog.string/format)

(comment
  "I'd like to define a 2048 component.
  Steps towards that goal:
    1. defrecord with all the state needed by this component including
       any subcomponents.
    2. Use only functional subcomponents: all functions must act as
       updaters or getters.
    3. Mark state that needs to be immutable.")

(def empty-board (vec (repeat 16 0)))

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

(def targets
  "Returns a sequence of board indicies, starting at a board edge and
  advancing in the opposite of direction, ending just before it would
  reach idx."
  (memoize
   (fn
     [idx direction]
     (vec (reverse (take-while some? (next (iterate direction idx))))))))

(def cells
  "Returns a sequence of board indicies specified by direction. Each specific
  sequence is designed so that values in the cells are only operated once by
  slide or combine operations."
  (memoize
   (fn [direction]
     (sort-by #(count (targets % direction)) (range 16)))))

(defn col
  "Return a column of board. Columns are referenced by idx from 0 to 3."
  [board idx]
  (map board (nth (partition 4 (cells left)) idx)))

(defprotocol Tiltable
  (get-cell- [_ idx not-found])
  (move-tile- [_ from to])
  (upgrade- [_ idx value] "Update a value, which is assumed negative to indicate 'merged'.")
  (clear-merged- [_] "Update every cell's value to its absolute value")
  (new-tile- [_ idx value]))

(extend-protocol Tiltable
  #+cljs cljs.core.PersistentVector #+clj clojure.lang.PersistentVector
  (get-cell- [v idx not-found] (get v idx not-found))
  (move-tile- [v from to] (assoc v from 0 to (nth v from)))
  (upgrade- [v idx value] (assoc v idx value))
  (clear-merged- [v] (mapv #(Math/abs %) v))
  (new-tile- [v idx value] (assoc v idx value)))

(defn find-blank
  "Return the index of the farthest blank cell in direction, starting at idx and
  with only blank cells between them. If on the edge or next to an existing
  cell, return idx."
  [board direction idx]
  (loop [cell idx]
    (let [new-cell (direction cell)]
      (if (zero? (get-cell- board new-cell -1))
        (recur new-cell)
        cell))))

(defn tilt-cell [board origin direction]
  (let [value (get-cell- board origin nil)]
    (-> board
        (->/when-not (zero? value)
          (->/let [target (find-blank board direction origin)]
            (->/let [merge-target (direction target)]
              (->/if (and merge-target (= value (get-cell- board merge-target nil)))
                (-> (move-tile- origin merge-target)
                    (upgrade- merge-target (* -2 value)))
                (->/when-not (= target origin)
                  (move-tile- origin target)))))))))

(defn tilt [board direction]
  (clear-merged-
   (reduce
    (fn [board origin]
      (tilt-cell board origin direction))
    board
    (cells direction))))

(defn find-blanks
  "Return a sequence of indicies for all blank cells found in board."
  [board]
  (vec (keep-indexed #(when (zero? %2) %1) board)))

(defn game-over? [board]
  (= board
     (tilt board up)
     (tilt board left)))

(def glyphs
  (zipmap
   (cons 0 (next (iterate #(* % 2) 1)))
   (cons \space '[🁤 🁥 🁦 🁧 🁨 🁩 🁰 🁷 🁾 🂅 🂌 🂓])))

(defn board-str [board over]
  (if over
    "Game over!"
    (apply str
           (apply format
                  (str "╭───────────╮\n"
                       "│%s %s %s %s│\n"
                       "│%s %s %s %s│\n"
                       "│%s %s %s %s│\n"
                       "│%s %s %s %s│\n"
                       "╰───────────╯")
                  (map #(cond
                         (zero? %) "  "
                         (<= % 64) (format "%2d" %)
                         (<= % 4096) (format "\033[95m%2d\033[0m" (/ % 64))
                         :else (format "\033[92m%2d\033[0m" (/ % 4096)))
                       board)))))

;; Rubber meets the road from this point on.
;; every function below here are updater functions on game.
(defrecord Game [^:immutable board, rng, player, observer])

(defprotocol Player
  (^:updater make-move- [self board-over])
  (get-move- [self]))

(defn pollute
  "Add a 2 (90% chance) or 4 (10% chance) to a random blank cell."
  [game]

  ;; with syntax support and updater functions only
  (->/do game
         (->/let [idx (by :rng sys/gen- (sys/rnd-nth (find-blanks (:board <>))))
                  val (by :rng sys/gen- (-> sys/num- (#(if (< % 0.1) 4 2))))]
           (assoc-in [:board idx] val))))

(def cmd-map
  {:up up
   :down down
   :left left
   :right right
   :quit :quit})

(defn play-turn
  "Play a single turn."
  [game]
  (->/do game
         (->/when-let [cmd (by :player
                               (make-move- (select-keys <> [:board :over]))
                               (-> get-move- cmd-map))]
           (->/if (= :quit cmd)
             (assoc :over true)
             (->/let [old-board (:board <>)
                      new-board (by :board (tilt cmd) identity)]
               (->/if (= old-board new-board)
                 (assoc :over (game-over? old-board))
                 pollute))))))

(defn play-game
  "Play an entire game."
  [game]

  (if (:over game)
    game
    (let [new-game (play-turn game)]
      #+clj (recur new-game)
      #+cljs (js/setTimeout #(play-game new-game) 0))))

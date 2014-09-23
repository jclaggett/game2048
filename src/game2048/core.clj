(ns game2048.core)

"I have a thought: using lonocloud.graph to navigate the array. The thought is
that left, right, up, down directions might be considered navigations. That is,
instead of using explicit links in a map, somehow use a protocol to express
implicit links. Just like traversing nodes in a graph, traversing the array
would provide the total array as a context.

No idea how to achieve the vision or even what that vision might be."

(def board
  [0,0,0,0
   0,2,0,2
   2,2,4,4
   0,8,0,0])

;; TODO These functions all hard code a 4x4 board maybe it would be better to
;; generate these functions based on the board dimensions.
(defn left
  "Return index left of idx or nil when at the edge."
  [idx]
  (when (not= 0 (rem idx 4))
    (- idx 1)))

(defn up [idx]
  "Return index above idx or nil when at edge."
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
        size 4
        ]
    (condp = direction
      left (map #(+ (int (/ % size)) (* size (rem % size)))
                idxs)
      right (reverse (cells left))
      down (reverse (cells up))
      up idxs)))

(defn find-empty-cell
  "Return the index of the farthest zero cell in direction, starting at idx and
  with only zero cells between them. If on the edge or next to an existing
  cell, return idx."
  [board direction idx]
  (loop [cell idx]
    (let [new-cell (direction cell)]
      (if (zero? (get board new-cell -1))
        (recur new-cell)
        cell))))

(defn slide
  "Slide each non-zero value in direction until stopped by another value or the
  edge."
  [board direction]
  (reduce
    (fn [board cell]
      (if (zero? (get board cell))
        board
        (let [new-cell (find-empty-cell board direction cell)]
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
        board)
      )
    board
    (cells direction))
  )

(defn tilt [board direction]
  "Tilt the board according to the specified direction.
   Tilting is done in three steps:
    1. slide all numbers in direction,
    2. merge same numbers in direction and
    3. slide all combined numbers in direction again."

  (-> board
      (slide direction)
      (combine direction)
      (slide direction)))

(defn rand-zero-cell
  "Return the index of a random zero cell or nil if no zero cell exists."
  [board]
  (->> board
       (map list (range))
       (filter #(zero? (second %)))
       shuffle
       ffirst))

(defn pollute
  "Add a value to index."
  [board]
  (if-let [idx (rand-zero-cell board)]
    (assoc board idx (if (zero? (rand-int 10)) 4 2))
    board))

(defn show
  "Print current board state."
  [board]
  (doseq [row (partition 4 board)]
    (println row))
  (println "_________")
  board)

()

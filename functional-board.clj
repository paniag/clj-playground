;
; GLOBALS
;

(def dim 8)

;initial board structure
(def initial-board
  (assoc
    (reduce
      (fn [b l]
        (assoc b l :empty))
      {}
      (for [x (range dim) y (range dim)] [x y]))
    [3 3] :white
    [4 4] :white
    [3 4] :black
    [4 3] :black))

;dirs are 0-7, starting at north and going clockwise
;these are the deltas in order to move one step in given dir
(defn dir-delta [x] ((mod x 8)
                       {0 [0 -1]
                        1 [1 -1]
                        2 [1 0]
                        3 [1 1]
                        4 [0 1]
                        5 [-1 1]
                        6 [-1 0]
                        7 [-1 -1]}))

(defn delta-loc 
  "returns the location one step in the given dir, or nil if dir moves off edge of board"
  [[x y] dir]
    (let [[dx dy] (dir-delta dir)
          [nx ny] [(+ x dx) (+ y dy)]]
      (when (check-bounds [nx ny])
        [nx ny])))

(defn check-bounds [[x y]]
  (if (and (< x dim) (<= 0 x) (< y dim) (<= 0 y))
    true
    false))

;argument should either be :white or :black
(defn other [p]
  (if (= p :white)
    :black
    :white))

;
; BOARD INSPECTION: valid moves, heuristics, etc
;

;collection of all valid moves
(defn all-moves [board player]
  (filter #(%)
          (for [row (range 8) col (range 8)]
            (valid-move? board player [row col]))))

(defn valid-move? [board player loc]
  (let [result (some #(valid-directed-move? player loc %)
                      (range 8))]
    (when result loc)))

;check if specific move exists
;caller has validated that loc is on the board and that space is blank
(defn valid-directed-move? [board player loc dir]
  (let [orig-loc loc
        new-loc (delta-loc loc dir)
        op (other player)]
    (when (and
            (is-blank board loc)
            new-loc
            (is-opponent board player new-loc))
      (loop [loc (delta-loc new-loc dir)]
          (when loc
            (if (is-opponent board player loc)
              (recur (delta-loc loc dir))
              (is-own board player loc)))))))

(defn is-blank [board loc]
  (= :empty (board loc)))

(defn is-own [board player loc]
  (= player (board loc)))

(defn is-opponent [board player loc]
  (= (other player) (board loc)))

;
; BOARD MUTATION: applying moves, updating board
;

;assumes move is valid
(defn apply-move [board player move]
  (reduce (fn [b d]
            (claim-pieces b player (delta-loc start d) d)))
          (assoc board move player)
          (range 8))

;assumes move is valid
(defn claim-pieces [b player s dir]
  (let [result
        (loop [board b
               start s]
          (if (and start (is-opponent board player start))
            (recur (assoc board start player)
                   (delta-loc start dir))
            [board start]))
        new-board (first result)
        final-loc (second result)]
    (if (and final-loc (is-own new-board player final-loc))
      new-board
      b)))

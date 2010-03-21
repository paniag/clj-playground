(def dim 8)

;board structure
(defstruct cell :piece)

(def board
  (apply vector
         (map (fn [_]
                (apply vector (map (fn [_] (ref (struct cell 'blank)))
                                   (range dim))))
              (range dim))))

;grab a square on the board
(defn place [[x y]]
  (-> board (nth x) (nth y)))

;what piece is at loc on board;
; 'blank, 'white, 'black
(defn piece [loc]
  (let [p (place loc)]
    (@p :piece)))

;argument should either be 'white or 'black
(defn other [p]
  (if (= p 'white)
    'black
    'white))

;list of all valid moves
(defn all-moves [player]
  (filter (fn [x] x)
          (for [row (range 8) col (range 8)]
            (valid-move? player [row col]))))

;returns loc if move exists; nil otherwise
(defn valid-move? [player loc]
  (some (fn [x] (valid-directed-move? player loc x))
        (range 8)))

;check if specific move exists
;caller has validated that loc is on the board and that space is blank
(defn valid-directed-move? [player loc dir]
  (let [new-loc (delta-loc loc dir)
        op (other player)]
    (when (and
            (is-blank loc)
            new-loc
            (= (piece new-loc) op))
      (loop [loc (delta-loc new-loc dir)]
          (when loc
            (if (= (piece loc) op)
              (recur (delta-loc loc dir))
              (when (= (piece loc) player) loc)))))))

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

;
; GLOBALS
;

(def dim 8)

(def
  #^{:doc "Standard Othello starting configuration."}
  initial-board
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

(defn piece-at-loc
  "Returns piece at location loc on board, or nil if loc is invalid."
  ([board loc]
    (board loc)))

;WARNING: unchecked indexing
(defn claim-loc
  "Returns a modification of board in which the piece at loc belongs to player."
  ([board player loc]
    (assoc board loc player)))

(defn dir-delta
  "Maps a direction 0-7 to a pair [dx dy] defining the direction."
  ([x] ({0 [0 -1]
         1 [1 -1]
         2 [1 0]
         3 [1 1]
         4 [0 1]
         5 [-1 1]
         6 [-1 0]
         7 [-1 -1]}
        (mod x 8))))

(defn valid-loc?
  "Validates a location, returning loc if valid and nil otherwise."
  ([loc]
    (let [[x y] loc]
      (when (and (<= 0 x) (< x dim)
                 (<= 0 y) (< y dim))
        loc))))

(defn neighbor 
  "Returns the location one step in direction dir, or nil if the new location is invalid."
  ([loc dir]
    (let [[x y]   loc
          [dx dy] (dir-delta dir)
          [nx ny] [(+ x dx) (+ y dy)]]
      (valid-loc? [nx ny]))))

(defn other
  "Returns the symbol representing player's opponent, or nil if the symbol player is invalid."
  ([player]
    (some (when (= player :white) :black)
          (when (= player :black) :white))))

;
; BOARD INSPECTION: valid moves, heuristics, etc
;

(defn is-blank?
  "True if neither player has a piece at loc on board, false otherwise."
  ([board loc]
    (= :empty (piece-at-loc board loc))))

(defn is-own?
  "True if player has a piece at loc on board, false otherwise."
  ([board player loc]
    (= player (piece-at-loc board loc))))

(defn is-opponent?
  "True if player's opponent has a piece at loc on board, false otherwise."
  ([board player loc]
    (is-own? board (other player) loc)))

;check if specific move exists
;caller has validated that loc is on the board and that space is blank
(defn valid-directed-move?
  "Checks whether any opponent pieces in direction dir would be captured if player
  put a piece at loc. Returns true if pieces would be captured, or false otherwise.
  
  Note: Assumes loc is empty."
  ([board player loc dir]
    (let [new-loc (neighbor loc dir)]
      (and new-loc
           (is-opponent? board player new-loc)
           (loop [new-loc (neighbor new-loc dir)]
             (if (and new-loc (is-opponent? board player new-loc))
               (recur (neighbor new-loc dir))
               (is-own? board player new-loc)))))))

(defn valid-move? [board player loc]
  (let [result (some #(valid-directed-move? board player loc %)
                      (range 8))]
    (when result loc)))

;collection of all valid moves
(defn all-moves [board player]
  (filter identity
          (for [row (range dim) col (range dim)]
            (valid-move? board player [row col]))))

;
; BOARD MUTATION: applying moves, updating board
;

;assumes move is valid
(defn claim-pieces [b player s dir]
  (let [result
        (loop [board b
               start s]
          (if (and start (is-opponent? board player start))
            (recur (claim-loc board player start)
                   (neighbor start dir))
            [board start]))
        new-board (first result)
        final-loc (second result)]
    (if (and final-loc (is-own? new-board player final-loc))
      new-board
      b)))

;assumes move is valid
(defn apply-move [board player loc]
  (if loc
    (reduce (fn [b d]
              (claim-pieces b player (neighbor loc d) d))
            (claim-loc board player loc)
            (range 8))
    board))

;
; GAMEPLAY
;

(defn first-valid-move [board player]
  (first (all-moves board player)))

(defn random-move [board player]
  (let [moves (all-moves board player)]
    (when (< 0 (count moves)) (nth moves (rand-int (count moves))))))

(defn play-turn [b p strategy]
  (apply-move b p (strategy b p)))

(defn face-off-turn [[b p s]]
  [(play-turn b p (s p)) (other p) s])

(defn face-off-game [b p s]
  (iterate face-off-turn [b p s]))

;
; DISPLAY
;

(def to-char {:empty "_" :white "O" :black "*"})

(defn show-board [board]
  (println (with-out-str
           (dotimes [x 8]
             (println (reduce (fn [a b] (str a b))
                          (for [y (range 8)]
                            (to-char (piece-at-loc board [x y]))))))))
  (flush))

(defn show-turn [game n]
  (show-board (first (nth game n))))

(defn show-game [game n t]
  (dotimes [i (inc n)] (show-turn game i) (. Thread (sleep t))))

;
; CONVENIENCES
;

(defn game1 [] (face-off-game initial-board :white {:white first-valid-move :black first-valid-move}))

(defn game2 [] (face-off-game initial-board :black {:white first-valid-move :black first-valid-move}))

(defn game3 [] (face-off-game initial-board :white {:white random-move :black random-move}))

(defn game4 [] (face-off-game initial-board :black {:white random-move :black random-move}))

(defn game5 [] (face-off-game initial-board :white {:white random-move :black first-valid-move}))

(defn game6 [] (face-off-game initial-board :black {:white random-move :black first-valid-move}))

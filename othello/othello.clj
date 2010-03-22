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
    (some identity
          [(when (= player :white) :black)
           (when (= player :black) :white)])))

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

(defn valid-directed-move?
  "Checks whether any opponent pieces in direction dir would be captured if player
  put a piece at loc. Returns true if pieces would be captured, or false otherwise.
  
  Note: Assumes there is not already a piece at loc."
  ([board player loc dir]
    (let [loc (neighbor loc dir)]
      (and loc
           (is-opponent? board player loc)
           (loop [loc (neighbor loc dir)]
             (if (and loc (is-opponent? board player loc))
               (recur (neighbor loc dir))
               (is-own? board player loc)))))))

(defn valid-move?
  "Returns loc if player can legally place a piece at loc on board, nil otherwise."
  ([board player loc]
    (when (is-blank? board loc)
      (let [result (some #(valid-directed-move? board player loc %)
                         (range 8))]
        (when result loc)))))

(defn all-moves
  "Find all legal moves for player on board."
  ([board player]
    (filter identity
            (for [row (range dim) col (range dim)]
              (valid-move? board player [row col])))))

;
; BOARD MUTATION: applying moves, updating board
;

(defn claim-pieces
  "Return a board with all pieces flipped that can be flipped along dir starting with loc."
  ([board player loc dir]
    (let [result (loop [board board
                        loc loc]
                   (if (and loc (is-opponent? board player loc))
                     (recur (claim-loc board player loc)
                            (neighbor loc dir))
                     [board loc]))
          new-board (first result)
          new-loc (second result)]
      (if (and new-loc (is-own? new-board player new-loc))
        new-board
        board))))

(defn apply-move
  "Returns board which is the result of placing player's piece at loc.
  If the move is invalid, the board is returned unchanged."
  ([board player loc]
    (if (is-blank? board loc)
      (reduce #(claim-pieces %1 player (neighbor loc %2) %2)
              (claim-loc board player loc)
              (range 8))
      board)))

;
; GAMEPLAY
;

(defn first-valid-move
  "Strategy which finds the first valid move for player."
  ([board player]
    (first (all-moves board player))))

(defn random-move
  "Strategy which picks a random valid move uniformly."
  ([board player]
    (let [moves (all-moves board player)]
      (when (< 0 (count moves)) (nth moves (rand-int (count moves)))))))

(defn play-turn
  "Chooses a move for player according to strategy and returns resulting board."
  ([board player strategy]
    (apply-move board player (strategy board player))))

(defn face-off-turn
  "Plays a for player according to the associated strategy in stragies,
  and returns the new game state. The composition of this function with
  itself n times returns the game state after n turns."
  ([[board player strategies]]
    [(play-turn board player (strategies player)) (other player) strategies]))

(defn face-off-game
  "Returns a lazy sequence of game states starting from the given state."
  ([board player strategies]
    (iterate face-off-turn [board player strategies])))

;
; DISPLAY
;

(def #^{:doc "Characters to use when printing board to console."}
  to-char
  {:empty "_" :white "O" :black "*"})

(defn show-board
  "Print a text representation of board to *out*."
  ([board]
    (println (with-out-str
             (dotimes [x 8]
               (println (reduce (fn [a b] (str a b))
                            (for [y (range 8)]
                              (to-char (piece-at-loc board [x y]))))))))
    (flush)))

(defn show-turn
  "Display the board after n turns of game."
  ([game n]
    (show-board (first (nth game n)))))

(defn show-game
  "Show n turns of game, pausing t ms between each turn."
  ([game n t]
    (dotimes [i (inc n)] (show-turn game i) (. Thread (sleep t)))))

;
; CONVENIENCES
;

(defn game1 [] (face-off-game initial-board :white {:white first-valid-move :black first-valid-move}))

(defn game2 [] (face-off-game initial-board :black {:white first-valid-move :black first-valid-move}))

(defn game3 [] (face-off-game initial-board :white {:white random-move :black random-move}))

(defn game4 [] (face-off-game initial-board :black {:white random-move :black random-move}))

(defn game5 [] (face-off-game initial-board :white {:white random-move :black first-valid-move}))

(defn game6 [] (face-off-game initial-board :black {:white random-move :black first-valid-move}))

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
(defn dir-delta [x] ({0 [0 -1]
                      1 [1 -1]
                      2 [1 0]
                      3 [1 1]
                      4 [0 1]
                      5 [-1 1]
                      6 [-1 0]
                      7 [-1 -1]}
                     (mod x 8)))

(defn check-bounds [[x y]]
  (if (and (< x dim) (<= 0 x) (< y dim) (<= 0 y))
    true
    false))

(defn delta-loc 
  "returns the location one step in the given dir, or nil if dir moves off edge of board"
  [[x y] dir]
    (let [[dx dy] (dir-delta dir)
          [nx ny] [(+ x dx) (+ y dy)]]
      (when (check-bounds [nx ny])
        [nx ny])))

;argument should either be :white or :black
(defn other [p]
  (if (= p :white)
    :black
    :white))

;
; BOARD INSPECTION: valid moves, heuristics, etc
;

(defn is-blank [board loc]
  (= :empty (board loc)))

(defn is-own [board player loc]
  (= player (board loc)))

(defn is-opponent [board player loc]
  (= (other player) (board loc)))

;check if specific move exists
;caller has validated that loc is on the board and that space is blank
(defn valid-directed-move? [board player loc dir]
  (let [new-loc (delta-loc loc dir)]
    (when (and
            (is-blank board loc)
            new-loc
            (is-opponent board player new-loc))
      (loop [loc (delta-loc new-loc dir)]
          (when loc
            (if (is-opponent board player loc)
              (recur (delta-loc loc dir))
              (is-own board player loc)))))))

(defn valid-move? [board player loc]
  (let [result (some #(valid-directed-move? board player loc %)
                      (range 8))]
    (when result loc)))

;collection of all valid moves
(defn all-moves [board player]
  (filter (fn [x] x)
          (for [row (range 8) col (range 8)]
            (valid-move? board player [row col]))))

;
; BOARD MUTATION: applying moves, updating board
;

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

;assumes move is valid
(defn apply-move [board player move]
  (if move
    (reduce (fn [b d]
              (claim-pieces b player (delta-loc move d) d))
            (assoc board move player)
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
; SILLY STUFF
;

(def to-char {:empty "-" :white "X" :black "O"})

(defn show-board [board]
  (println (with-out-str
           (dotimes [x 8]
             (println (reduce (fn [a b] (str a b))
                          (for [y (range 8)]
                            (to-char (board [x y]))))))))
  (flush))

(defn show-game [game n]
  (show-board (first (nth game n))))

(defn show-turns [game n t]
  (dotimes [i (inc n)] (show-game game i) (. Thread (sleep t))))

(def game (face-off-game initial-board :white {:white first-valid-move :black first-valid-move}))

(def game2 (face-off-game initial-board :black {:white first-valid-move :black first-valid-move}))

(def game3 (face-off-game initial-board :white {:white random-move :black random-move}))

(def game4 (face-off-game initial-board :black {:white random-move :black random-move}))

(def game5 (face-off-game initial-board :white {:white random-move :black first-valid-move}))

(def game6 (face-off-game initial-board :black {:white random-move :black first-valid-move}))

(def dim 8)

;board structure
(defstruct cell :piece)

(def board
  (apply vector
         (map (fn [_]
                (apply vector (map (fn [_] (ref (struct cell 'blank)))
                                   (range dim))))
              (range dim))))

(defn place [[x y]]
  (-> board (nth x) (nth y)))

;check if specific move exists
(defn exists-move [loc dir]
  (let [p (place loc)]
    (if )))

;dirs are 0-7, starting at north and going clockwise
;these are the deltas in order to move one step in given dir
(def dir-delta {0 [0 -1]
                1 [1 -1]
                2 [1 0]
                3 [1 1]
                4 [0 1]
                5 [-1 1]
                6 [-1 0]
                7 [-1 -1]})

(defn delta-loc 
  "returns the location one step in the given dir, or nil if dir moves off edge of board"
  [[x y] dir]
    (let [[dx dy] (dir-delta (bound 8 dir))
          [nx ny] [(+ x dx) (+ y dy)]]
      (when (and (< nx dim) (< ny dim)
                 (<= 0 nx) (<= 0 ny))
        [nx ny])))

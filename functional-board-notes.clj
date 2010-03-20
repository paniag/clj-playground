(def dim 8)

;first idea: board as dictionary
(def board
  (reduce (fn [x y] (assoc y 'blank))
          (for [x (range dim) y (range dim)])))



;second idea: board as dictionary of lines
;a line is a set of keys to the board, ie
;  locations, which form a contiguous line




;third idea: board as sets of lines

;check if two lines intersect, returning location of intersection or nil
;split a line in 2 by removing a given point, returning the set of new lines resulting
;check endpoints of lines
;add a line between given endpoints

;a line is a set of locations

(defstruct line-struct :dir :locs)

(defn intersection-point [line1 line2]
  (let [ans (intersection
              (:locs line1)
              (:locs line2))]
    (when ans
      (first ans))))

(defn split-line [line loc]
  (set
    (filter
      (fn [x] (< 0 (count x)))
      [(select (fn [x] (loc-lt x loc (:dir line))) line)
       (select (fn [x] (loc-lt loc x (:dir line))) line)]))

;a line is a struct with direction and endpoints

(defstruct line :dir :start :end)

(defn intersection [line1 line2]
  (cond
    (and (contains? [:vert :horz] (:dir line1))
         (contains? [:up :down] (:dir line2)))
      )

(defn interval-intersect? [[a b] [c d]]
  (or (and (<= a d) (<= c a))
      (and (<= b d) (<= c b))))

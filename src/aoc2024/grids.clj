(ns aoc2024.grids
  (:require [clojure.string :as str]))

(def parse-grid
  "takes seq of strings (lines), ensures that it's indexable by converting to vec.
  This would potentially be much faster if flattened into a 1D vector, but for now w/e"
  vec)

(defn grid-get
  "(grid-get g [r c]) gets the value stored in the r'th row and c'th column of grid g
  (grid-get g r c) is similar"
  ([g idxs] (get-in g idxs))
  ([g row col] (get-in g [row col])))

(def DIRS4
  "directions in 2D space"
  [:up :right :down :left])

(def flip-dir
  "Inverts the supplied direction"
  {:up :down
   :right :left
   :down :up
   :left :right})

(defn move
  "(move dir n [row col]) creates a new point [r' c'] where r' and c' are the coordinates
  of moving n steps in direction dir (see DIRS4)"
  [dir n [row col]]
  (case dir
    :up [(- row n) col]
    :down [(+ row n) col]
    :left [row (- col n)]
    :right [row (+ col n)]))

(defn width [grid] (count (first grid)))
(defn height [grid] (count grid))

(defn coords
  "Returns a lazy sequence of all indexable coordinates in grid"
  [grid]
  (for [row (range (height grid))
        col (range (width grid))]
    [row col]))

(defn in-grid?
  ([g [row col]] (and (<= 0 row (dec (height g)))
                      (<= 0 col (dec (width g)))))
  ([g row col] (in-grid? g [row col])))

(defn grid-update
  "Mutates grid g at coordinates ij with function f and returns a new grid"
  [g ij f & args] (apply update-in g ij f args))

(defn grid-set
  "Sets coordinate ij in grid g to x"
  ([g ij x] (assoc-in g ij x))
  ([g row col x] (assoc-in g [row col] x)))

(defn neighbors4
  "Returns neighbors above, below, to the left of, and to the right of coord ij.
  If g is supplied, only returns coordinates that are within g."
  ([ij] (mapv #(move % 1 ij) DIRS4))
  ([ij g] (into [] (comp (map #(move % 1 ij))
                         (filter #(in-grid? g %)))
                DIRS4))
  ([g row col] (neighbors4 g [row col])))

(defn coords-of
  "searches grid g for x and returns the 2D coordinate of x"
  [g x]
  (if (string? (first g))
    (->> (map-indexed (fn [rowdx row] (some->> (str/index-of row x) (vector rowdx))) g)
         (some identity))
    (->> g
         (map-indexed (fn [rowdx row]
                        (->> (map vector (range) row)
                             (some (fn [[coldx y]] (and (= x y) [rowdx coldx]))))))
         (some identity))))

(defn perimeter
  "returns all coordinates on the outside edge of the grid. optionally includes corners"
  [g & {:keys [corners?] :or {corners? false}}]
  (concat (for [col (range (width g))] [-1 col])
          (for [row (range (height g))] [row (width g)])
          (for [col (range (width g))] [(height g) col])
          (for [row (range (height g))] [row -1])
          (if corners?
            [[-1 -1] [-1 (width g)] [(height g) (width g)] [(height g) -1]]
            [])))

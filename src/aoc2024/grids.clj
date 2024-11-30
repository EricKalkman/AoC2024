(ns aoc2024.grids
  (:require [clojure.string :as str]))

(def parse-grid
  "takes seq of strings (lines), ensures that it's indexable by converting to vec.
  This would potentially be much faster if flattened into a 1D vector, but for now w/e"
  vec)

(defn grid-get
  ([g idxs] (get-in g idxs))
  ([g row col] (get-in g [row col])))

(def DIRS4
  "directions in 2D space"
  [:up :right :down :left])

(def flip-dir
  {:up :down
   :right :left
   :down :up
   :left :right})

(defn move [dir n [row col]]
  (case dir
    :up [(- row n) col]
    :down [(+ row n) col]
    :left [row (- col n)]
    :right [row (+ col n)]))

(defn width [grid] (count (first grid)))
(defn height [grid] (count grid))

(defn coords [grid]
  (for [row (range (height grid))
        col (range (width grid))]
    [row col]))

(defn in-grid?
  ([g [row col]] (and (<= 0 row (dec (height g)))
                      (<= 0 col (dec (width g)))))
  ([g row col] (in-grid? g [row col])))

(defn grid-update
  [g ij f & args] (apply update-in g ij f args))

(defn grid-set
  ([g ij x] (assoc-in g ij x))
  ([g row col x] (assoc-in g [row col] x)))

(defn neighbors4
  ([ij] (mapv #(move % 1 ij) DIRS4))
  ([g ij] (into [] (comp (map #(move % 1 ij))
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

(ns aoc2024.day06
  (:require [clojure.string :as str]
            [aoc2024.grids :as g]))

(defn neighbor [grid [coord dir]]
  (let [next-coord (g/move dir 1 coord)]
    (cond
      (not (g/in-grid? grid next-coord)) nil
      (= \# (g/grid-get grid next-coord)) [coord (g/turn-right dir)]
      :else [next-coord dir])))

(defn nodes-visited [grid start]
  (loop [node [start :up]
         visited? #{start}]
    (if-let [neigh (neighbor grid node)]
      (recur neigh (conj visited? (first neigh)))
      visited?)))

(defn part-1 [lines]
  (let [grid (g/parse-grid lines)
        start (g/coords-of grid \^)]
    (->> (nodes-visited grid start)
         count)))

(defn next-turn [grid [coord dir]]
  (let [next-coord
        (->> (iterate #(g/move dir 1 %) coord)
             (drop-while #(and (g/in-grid? grid %)
                               (not= \# (g/grid-get grid %))))
             first)]
    (and (g/in-grid? grid next-coord)
      [(g/move dir -1 next-coord) (g/turn-right dir)])))

(defn in-cycle? [grid start]
  (loop [node [start :up]
         visited? #{[start :up]}]
    (if-let [neigh (next-turn grid node)]
      (if (visited? neigh)
        true
        (recur neigh (conj visited? neigh)))
      false)))

(defn part-2 [lines]
  (let [grid (g/parse-grid (map vec lines))
        start (g/coords-of grid \^)
        visited? (nodes-visited grid start)]
    (->> visited?
         ; hehehe
         (pmap #(in-cycle? (g/grid-set grid % \#) start))
         (filter identity)
         count)))

(comment
  (def test-inp "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

  (def lines (str/split-lines test-inp))

  (part-1 lines) ; 41
  (part-1 (str/split-lines (slurp "inputs/day06.inp"))) ; 4696

  (part-2 lines) ; 6
  (time (part-2 (str/split-lines (slurp "inputs/day06.inp")))) ; 1443
  )

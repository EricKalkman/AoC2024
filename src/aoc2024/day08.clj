(ns aoc2024.day08
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as m]))

(defn collect-antennae [grid]
  (->> (g/coords grid)
       (filter #(not= \. (g/grid-get grid %)))
       (group-by #(g/grid-get grid %))))

(defn coords-in-line
  "Calculates all points in grid with integer coordinates on a line from a to b starting from a."
  [grid a b]
  (let [[dr dc] (g/coord- b a)
        denom (m/gcd (abs dr) (abs dc))
        delta [(/ dr denom) (/ dc denom)]]
    (->> a
         (iterate (fn [coord] (g/coord+ coord delta)))
         (take-while #(g/in-grid? grid %)))))

(defn first-double-dist [a b coords]
  (let [[dr dc] (g/coord- b a)
        denom (m/gcd (abs dr) (abs dc))]
    (nth coords (* 2 denom) nil)))

(defn combinations
  "Returns all n_C_2 combinations of as."
  [as]
  (->> as
       (iterate rest)
       (take-while #(seq (rest %))) ; while > 1 element
       (mapcat #(map (partial vector (first %)) (rest %)))))

(defn antinodes-of [which grid a b]
  ; only take the first frequency (-> antinode is twice the distance from a
  ; as b and vice versa)
  (->>
    (case which
      :first
      (->> [[a b] [b a]]
           (keep (fn [[x y]] (first-double-dist x y (coords-in-line grid x y)))))
      :all
      (concat (coords-in-line grid a b) (coords-in-line grid b a)))
    (into #{})))

(defn all-antinodes [which grid as]
  (->> (combinations as)
       (mapcat #(apply antinodes-of which grid %))
       (into #{})))

(defn part [which lines]
  (let [grid (g/parse-grid lines)
        ants (collect-antennae grid)]
    (->> (vals ants)
         (map #(all-antinodes which grid %))
         (reduce #(into %1 %2) #{})
         count)))

(def part-1 (partial part :first))
(def part-2 (partial part :all))

(comment
  (def test-inp "..........
..........
..........
....a.....
........a.
.....a....
..........
..........
..........
..........")

  (def test-inp2 "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

  (def l1 (str/split-lines test-inp))
  (def l2 (str/split-lines test-inp2))
  (def g1 (g/parse-grid l1))
  (def g2 (g/parse-grid (str/split-lines test-inp2)))

  (collect-antennae g1) ; {\a [[3 4] [4 8] [5 5]]}
  (collect-antennae g2) ; {\0 [[1 8] [2 5] [3 7] [4 4]], \A [[5 6] [8 8] [9 9]]}

  (let [coords (collect-antennae g1)
        as (coords \a)]
    (all-antinodes :first g1 as)) ; #{[7 6] [1 3] [2 0] [6 2]}

  (part-1 l1) ; 4
  (part-1 l2) ; 14
  (part-1 (str/split-lines (slurp "inputs/day08.inp"))) ; 329

  (def test-inp3 "T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
..........")
  (def l3 (str/split-lines test-inp3))

  (part-2 l3) ; 9
  (part-2 l2) ; 34
  (part-2 (str/split-lines (slurp "inputs/day08.inp"))) ; 1190
)

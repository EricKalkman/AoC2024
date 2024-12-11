(ns aoc2024.day10
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [aoc2024.searches :as search]))

(defn parse-input [lines]
  ; note: Character/digit returns -1 for non-digit characters, which is fine here
  ; given the monotonicity of edges
  (mapv #(mapv (fn [c] (Character/digit c 10)) %) lines))

(defn neighbors [grid pos]
  (let [cur-height (g/grid-get grid pos)]
    (if (= 9 cur-height)
      []
      (->> (g/neighbors4 pos grid)
           (filterv #(= (inc cur-height) (g/grid-get grid %)))))))

(defn trail-score [grid neighfunc pos-indexer start]
  (->> (search/bfs neighfunc start (constantly false))
       :prevs
       keys
       (filter #(= 9 (g/grid-get grid (pos-indexer %))))
       count))

(defn part-1 [lines]
  (let [grid (parse-input lines)]
    (->> (g/coords grid)
         (filter #(zero? (g/grid-get grid %)))
         (map (partial trail-score grid (partial neighbors grid) identity))
         (reduce +))))

(defn part-2 [lines]
  (let [grid (parse-input lines)]
    (->> (g/coords grid)
         (filter #(zero? (g/grid-get grid %)))
         (map (comp (partial trail-score grid
                             (comp #(mapv (fn [neigh] [neigh (gensym)]) %)
                                   (partial neighbors grid)
                                   first)
                             first)
                    #(vector % (gensym))))
         (reduce +))))

(comment
  (def test-inp "0123
1234
8765
9876")
  (def test-inp2 "...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9")

  (def test-inp3 "..90..9
...1.98
...2..7
6543456
765.987
876....
987....")

  (def test-inp4 "10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01")

  (def test-inp5 "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

  (part-1 (str/split-lines test-inp)) ; 1
  (part-1 (str/split-lines test-inp2)) ; 2
  (part-1 (str/split-lines test-inp3)) ; 4
  (part-1 (str/split-lines test-inp4)) ; 3

  (part-1 (str/split-lines test-inp5)) ; 36
  (part-1 (str/split-lines (slurp "inputs/day10.inp"))) ; 646

  (def test-inp6 ".....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9....")
  (def test-inp7
"..90..9
...1.98
...2..7
6543456
765.987
876....
987....")
  (part-2 (str/split-lines test-inp6)) ; 3
  (part-2 (str/split-lines test-inp7)) ; 13

  (part-2 (str/split-lines (slurp "inputs/day10.inp"))) ; 1494
  )

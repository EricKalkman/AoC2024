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

(defn visit-smarter [neighfunc coords]
  (fn
    ([] coords)
    ([_ cur]
     (let [cur-count (coords cur)]
       (visit-smarter
         neighfunc
         (reduce #(update %1 %2 (fn [c] (+ (or c 0) cur-count)))
                 coords
                 (neighfunc cur)))))))

(defn call [f] (f))

(defn trail-rating-smarter [grid start]
  (let [counts (->> (search/bfs (partial neighbors grid) start (constantly false)
                                (visit-smarter (partial neighbors grid) {start 1}))
                    :visit
                    call)]
    (->> counts
         keys
         (filter #(= 9 (g/grid-get grid %)))
         (map counts)
         (reduce +))))

(defn part-2-smarter [lines]
  (let [grid (parse-input lines)]
    (->> (g/coords grid)
         (filter #(zero? (g/grid-get grid %)))
         (map (partial trail-rating-smarter grid))
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

  (def g6 (parse-input (str/split-lines test-inp6)))
  (def g7 (parse-input (str/split-lines test-inp7)))

  (trail-rating-smarter g7 [0 3])

  (part-2 (str/split-lines (slurp "inputs/day10.inp"))) ; 1494
  (part-2-smarter (str/split-lines (slurp "inputs/day10.inp"))) ; 1494

  (def test-inp-boojum "9999999999999999999
9999999998999999999
9999999987899999999
9999999876789999999
9999998765678999999
9999987654567899999
9999876543456789999
9998765432345678999
9987654321234567899
9876543210123456789
9987654321234567899
9998765432345678999
9999876543456789999
9999987654567899999
9999998765678999999
9999999876789999999
9999999987899999999
9999999998999999999
9999999999999999999")

  (part-2 (str/split-lines test-inp-boojum)) ; 2044
  )

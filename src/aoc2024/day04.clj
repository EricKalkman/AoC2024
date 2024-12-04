(ns aoc2024.day04
  (:require [clojure.string :as str]
            [aoc2024.grids :as g]))

(defn check-word
  "Moves from cur-pos with steps along directions dirs in grid, checking if
   the encountered letters constitute the string word"
  [dirs word grid cur-pos]
  (->> (range (count word))
       (map #(reduce (fn [pos dir] (g/move dir % pos)) cur-pos dirs))
       (map #(g/grid-get grid %))
       (= (seq word))))

(def DIAGONAL-CHECKERS
  (mapv #(partial check-word %)
        [[:up] [:right] [:down] [:left]
         [:up :left] [:up :right] [:down :left] [:down :right]]))

(defn part [word checkers lines]
  (let [grid (g/parse-grid lines)]
    (->> (g/coords grid)
         (map #(->> (filter (fn [checker] (checker word grid %)) checkers)
                    count))
         (reduce +))))

(def part-1 (partial part "XMAS" DIAGONAL-CHECKERS))

(defn cross-checker [d1 d2 word grid cur-pos]
  (let [p1 (reduce (fn [coord dir] (g/move (g/flip-dir dir) (quot (count word) 2) coord))
                   cur-pos d1)
        p2 (reduce (fn [coord dir] (g/move (g/flip-dir dir) (quot (count word) 2) coord))
                   cur-pos d2)]
    (and (check-word d1 word grid p1)
         (check-word d2 word grid p2))))

(def MAS-CHECKERS
  (mapv #(partial cross-checker %1 %2)
        [[:down :right] [:down :right] [:up :left]  [:up :left]]
        [[:down :left]  [:up :right]   [:up :right] [:down :left]]))

(def part-2 (partial part "MAS" MAS-CHECKERS))

(comment
  (def test-inp "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

  (def lines (str/split-lines test-inp))

  (def grid (g/parse-grid lines))

  (part-1 lines) ; 18
  (part-1 (str/split-lines (slurp "inputs/day04.inp"))) ; 2297

  (def test-inp2 ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........")

  (def XMAS-KERNEL [[[-1 -1] \M] [[-1 1] \S]
                    [[0 0] \A]
                    [[1 -1] \M] [[1 1] \S]])


  (def lines2 (str/split-lines test-inp2))
  (def grid2 (g/parse-grid lines2))

  (part-2 grid2) ; 9
  (part-2 (str/split-lines (slurp "inputs/day04.inp"))) ; 1745
  )

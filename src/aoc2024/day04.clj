(ns aoc2024.day04
  (:require [clojure.string :as str]
            [aoc2024.grids :as g])
  (:import [aoc2024.grids vec2 Grid]))

(defn check-word
  "Moves from cur-pos with steps along directions dirs in grid, checking if
   the encountered letters constitute the string word"
  [dirs ^String word ^Grid grid ^vec2 cur-pos]
  (->> (range (count word))
       (map (comp #(grid % nil)
                  ; move once in all supplied directions
                  #(reduce (fn [^vec2 pos dir] (g/move dir % pos)) ^vec2 cur-pos dirs)))
       (= (seq word))))

(def DIAGONAL-CHECKERS
  (mapv #(partial check-word %)
        [[:up] [:right] [:down] [:left]
         [:up :left] [:up :right] [:down :left] [:down :right]]))

(defn part [word checkers lines]
  (let [grid (g/lines->grid lines)]
    (->> grid
         (g/fold-grid-keys
           #(->> checkers
                 (filter (fn [checker] (checker word grid %2)))
                 count
                 (+ %1))
           0))))

(def part-1 (partial part "XMAS" DIAGONAL-CHECKERS))

(defn cross-checker [d1 d2 ^String word ^Grid grid ^vec2 cur-pos]
  (let [p1 (reduce (fn [^vec2 coord dir]
                     (g/move (g/flip-dir dir) (quot (count word) 2) coord))
                   cur-pos d1)
        p2 (reduce (fn [^vec2 coord dir]
                     (g/move (g/flip-dir dir) (quot (count word) 2) coord))
                   cur-pos d2)]
    (and (check-word d1 word grid p1)
         (check-word d2 word grid p2))))

(def MAS-CHECKERS
  (mapv #(partial cross-checker %1 %2)
        [[:down :right] [:down :right] [:up :left]  [:up :left]]
        [[:down :left]  [:up :right]   [:up :right] [:down :left]]))

(def part-2 (partial part "MAS" MAS-CHECKERS))

(comment
  (def test-lines (str/split-lines (slurp "inputs/day04.test")))
  (part-1 test-lines) ; 18

  (time (part-1 (str/split-lines (slurp "inputs/day04.inp")))) ; 2297 ; ~170 ms

  (time (part-2 (str/split-lines (slurp "inputs/day04.inp")))) ; 1745 ;~140 ms
  )

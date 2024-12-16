(ns aoc2024.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2024.day01 :as day01]
            [aoc2024.day02 :as day02]
            [aoc2024.day03 :as day03]
            [aoc2024.day04 :as day04]
            [aoc2024.day05 :as day05]
            [aoc2024.day06 :as day06]
            [aoc2024.day07 :as day07]
            [aoc2024.day08 :as day08]
            [aoc2024.day09 :as day09]
            [aoc2024.day10 :as day10]
            [aoc2024.day11 :as day11]
            [aoc2024.day12 :as day12]
            [aoc2024.day13 :as day13]
            [aoc2024.day14 :as day14]
            [aoc2024.day15 :as day15]
            [aoc2024.day16 :as day16]
            ))

(defn do-file-lines [fname f]
  (if (.exists (io/file fname))
    (with-open [reader (io/reader fname)]
      (f (line-seq reader)))
    nil))

(defn do-part [day p1 p2]
  (println (format "DAY %02d" day))
  (println "Part 1: " (time (p1)))
  (println "Part 2: " (time (p2)))
  (println ""))

(defn line-thunks [day p1 p2]
  (if-let [lines (do-file-lines (format "inputs/day%02d.inp" day) #(into [] %))]
    [#(p1 lines) #(p2 lines)]
    nil))

(defn str-thunks [day p1 p2 & {:keys [trim] :or {trim false}}]
  (let [fname (format "inputs/day%02d.inp" day)]
    (if (.exists (io/file fname))
      (let [untrimmed (slurp fname)
            inp (if trim (str/trim untrimmed) untrimmed)]
        [#(p1 inp) #(p2 inp)])
      nil)))

(defn -main [& _]
  (some->> (line-thunks 1 day01/part-1 day01/part-2) (apply do-part 1))
  (some->> (line-thunks 2 day02/part-1 day02/part-2) (apply do-part 2))
  (some->> (str-thunks 3 day03/part-1 day03/part-2) (apply do-part 3))
  (some->> (line-thunks 4 day04/part-1 day04/part-2) (apply do-part 4))
  (some->> (str-thunks 5 day05/part-1 day05/part-2) (apply do-part 5))
  (some->> (line-thunks 6 day06/part-1 day06/part-2) (apply do-part 6))
  (some->> (str-thunks 7 day07/part-1 day07/part-2) (apply do-part 7))
  (some->> (line-thunks 8 day08/part-1 day08/part-2) (apply do-part 8))
  (some->> (str-thunks 9 day09/part-1 day09/part-2) (apply do-part 9))
  (some->> (line-thunks 10 day10/part-1 day10/part-2-smarter) (apply do-part 10))
  (some->> (str-thunks 11 day11/part-1 day11/part-2) (apply do-part 11))
  (some->> (line-thunks 12 day12/part-1 day12/part-2) (apply do-part 12))
  (some->> (str-thunks 13 day13/part-1 day13/part-2) (apply do-part 13))
  (some->> (str-thunks 14 day14/part-1 day14/part-2) (apply do-part 14))
  (some->> (str-thunks 15 day15/part-1 day15/part-2) (apply do-part 15))
  (some->> (str-thunks 16 day16/part-1 day16/part-2) (apply do-part 16))
  (shutdown-agents)
  )

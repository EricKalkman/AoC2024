(ns aoc2024.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2024.day01 :as day01]
            [aoc2024.day02 :as day02]
            [aoc2024.day03 :as day03]
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
  )

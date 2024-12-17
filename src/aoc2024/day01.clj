(ns aoc2024.day01
  (:require [clojure.string :as str]))

(defn part-1 [lines]
  (->> lines
       (map #(mapv parse-long (str/split % #" +")))
       (apply map (comp sort vector)) ; transpose to get lists, then sort
       (apply map (comp abs -))       ; un-transpose and find the abs diff between each element
       (reduce +)))                   ; sum

(defn part-2 [lines]
  (let [[a b] (->> lines
                    (map #(mapv parse-long (str/split % #" +")))
                    (apply map vector))]  ; get transposed lists
    (as-> b x
      ; count each element in b, storing counts in a map
      (reduce (fn [table n] (update table n #(inc (or % 0)))) {} x)
      ; multiply each element of a with its count in b
      (map #(* (x % 0) %) a)
      ; sum
      (reduce + x))))

(comment
  (def puzzle-inp (str/split-lines (slurp "inputs/day01.inp")))
  (part-1 puzzle-inp) ; 2176849

  (part-2 puzzle-inp) ; 23384288
)

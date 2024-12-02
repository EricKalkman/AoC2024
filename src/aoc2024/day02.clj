(ns aoc2024.day02
  (:require [clojure.string :as str]))

(defn parse-report [line] (->> (re-seq #"\d+" line) (mapv parse-long)))

(defn report-safe? [min-diff max-diff report]
  (let [diff1 (- (first report) (second report))]
    (->> (map - report (rest report))
         (every? #(and (> (* diff1 %) 0)
                       (<= min-diff (abs %) max-diff))))))

(defn part-1 [lines]
  (->> lines
       (filter (comp (partial report-safe? 1 3) parse-report))
       count))

(defn skip-nth [n col]
  (let [[a b] (split-at n col)]
    (concat a (rest b))))

(defn part-2 [lines]
  (let [{safe-0 true unsafe false} (group-by (partial report-safe? 1 3)
                                             (map parse-report lines))
        safe-1 (->> unsafe
                    (filter #(some (fn [idx] (report-safe? 1 3 (skip-nth idx %)))
                                   (range (count %)))))]
    (+ (count safe-0) (count safe-1))))

(comment
  (def test-inp "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

  (def lines (str/split-lines test-inp))

  (part-1 lines) ; 2
  (part-1 (str/split-lines (slurp "inputs/day02.inp"))) ; 663

  (part-2 lines) ; 4
  (part-2 (str/split-lines (slurp "inputs/day02.inp"))) ; 692
  )

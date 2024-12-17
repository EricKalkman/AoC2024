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
  (->> lines
       (map parse-report)
       (filter #(some (fn [idx] (report-safe? 1 3 (skip-nth idx %)))
                      (range (count %))))
       count))

(comment
  (part-1 (str/split-lines (slurp "inputs/day02.inp"))) ; 663

  (part-2 (str/split-lines (slurp "inputs/day02.inp"))) ; 692
  )

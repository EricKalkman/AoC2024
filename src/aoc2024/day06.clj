(ns aoc2024.day06
  (:require [clojure.string :as str]
            [aoc2024.grids :as g]))

(defn neighbor [grid [coord dir]]
  (let [next-coord (g/move dir 1 coord)]
    (cond
      (not (g/in-grid? grid next-coord)) nil
      (= \# (g/grid-get grid next-coord)) [coord (g/turn-right dir)]
      :else [next-coord dir])))

(defn nodes-visited [grid start]
  (loop [node [start :up]
         visited? {start :up}]
    (if-let [neigh (neighbor grid node)]
      (recur neigh (update visited? (first neigh) #(or % (second neigh))))
      visited?)))

(defn part-1 [lines]
  (let [grid (g/parse-grid lines)
        start (g/coords-of grid \^)]
    (->> (nodes-visited grid start)
         count)))

(defn next-turn [grid [coord dir]]
  (loop [coord (g/move dir 1 coord)]
    (if-let [^char c (g/grid-get grid coord)]
      (if (= \# c)
        [(g/move dir -1 coord) (g/turn-right dir)]
        (recur (g/move dir 1 coord)))
      nil)))

(defn in-cycle? [grid start]
  (loop [node start
         visited? #{start}]
    (if-let [neigh (next-turn grid node)]
      (if (visited? neigh)
        true
        (recur neigh (conj visited? neigh)))
      false)))

(defn part-2 [lines]
  (let [grid (g/parse-grid (map vec lines))
        start (g/coords-of grid \^)
        visited? (nodes-visited grid start)]
    (->> (dissoc visited? start)
         keys
         ; hehehe
         (pmap #(let [dir (visited? %)
                      start (g/move dir -1 %)]
                  (in-cycle? (g/grid-set grid % \#) [start dir])))
         (filter identity)
         count)))

(comment
  (part-1 (str/split-lines (slurp "inputs/day06.inp"))) ; 4696

  (time (part-2 (str/split-lines (slurp "inputs/day06.inp")))) ; 1443
  )

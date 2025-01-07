(ns aoc2024.day06
  (:require [clojure.string :as str]
            [aoc2024.grids :as g])
  (:import [aoc2024.grids vec2 Grid]
           [clojure.lang IPersistentVector]))

(defn neighbor [^Grid grid [^vec2 coord dir]]
  (let [next-coord ^vec2 (g/move dir 1 coord)]
    (cond
      (not (contains? grid next-coord)) nil
      (= \# ^char (grid next-coord)) [coord (g/turn-right dir)]
      :else [next-coord dir])))

(defn nodes-visited [^Grid grid ^vec2 start]
  (loop [node [start :up]
         visited? {start :up}]
    (if-let [neigh (neighbor grid node)]
      (recur neigh (update visited? (first neigh) #(or % (second neigh))))
      visited?)))

(defn part-1 [lines]
  (let [grid (g/lines->grid lines)
        start (g/first-coord-of grid #(= ^char % \^))]
    (->> (nodes-visited grid start)
         count)))

(defn next-turn [^Grid grid [^vec2 coord dir]]
  (loop [coord ^vec2 (g/move dir 1 coord)]
    (if-let [^char c (grid coord nil)]
      (if (= \# c)
        [(g/move dir -1 coord) (g/turn-right dir)]
        (recur (g/move dir 1 coord)))
      nil)))

(defn in-cycle? [^Grid grid ^IPersistentVector start]
  (loop [node start
         visited? #{start}]
    (if-let [neigh (next-turn grid node)]
      (if (visited? neigh)
        true
        (recur neigh (conj visited? neigh)))
      false)))

(defn part-2 [lines]
  (let [grid ^Grid (g/lines->grid lines)
        start ^vec2 (g/first-coord-of grid #(= \^ ^char %))
        visited? (nodes-visited grid start)]
    (->> (dissoc visited? start)
         (pmap #(let [[obst-coord dir] %
                      start ^vec2 (g/move dir -1 obst-coord)]
                  (or (in-cycle? (assoc ^Grid grid ^vec2 obst-coord \#) [start dir]) nil)))
         (filter identity)
         count)))

(comment
  (time (part-1 (str/split-lines (slurp "inputs/day06.inp")))) ; 4696

  (time (part-2 (str/split-lines (slurp "inputs/day06.inp")))) ; 1443
  )

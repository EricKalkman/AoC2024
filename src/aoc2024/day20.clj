(ns aoc2024.day20
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [aoc2024.searches :as search]
            [structural.core :as s]
            [aoc2024.grids :as g]
            [clojure.string :as str])
  (:import (aoc2024.grids vec2 Grid)))

(defn parse-input [lines]
  (let [grid (g/lines->grid lines)
        start (g/first-coord-of grid #(= \S %))
        end (g/first-coord-of grid #(= \E %))]
    {:grid (-> grid
               (assoc start \.)
               (assoc end \.))
     :start start
     :end end}))

(defn plain-neighbors [^Grid grid ^vec2 coord]
  ; don't need to check if in grid because surrounded by walls
  (->> (g/neighbors4 coord)
       (filterv #(= \. ^char (grid ^vec2 %)))))

(defn trace-path [prevs dst src]
  (concat
    (->> (iterate prevs dst)
         (take-while #(not= % src)))
    [src]))

(defn manh-dist
  ([[r1 c1] [r2 c2]] (manh-dist r1 c1 r2 c2))
  ([r1 c1 r2 c2]
   (+ (abs (- r1 r2)) (abs (- c1 c2)))))

(defn circle-coords [[r c] radius]
  (for [row (range (- r radius) (inc (+ r radius)))
        col (range (- c (- radius (abs (- row r))))
                   (inc (+ c (- radius (abs (- r row))))))]
    (g/->vec2 row col)))

(defn fold-circle-coords [f init ^Long h ^Long w ^Long radius ^vec2 coord]
  (s/with-slots [{:fields [row col]} ^vec2 coord
                 r row c col
                 max-row (inc (+ r radius))]
    (loop [acc init
           row (- r radius)
           col c
           max-col (inc c)]
      (cond
        (>= row max-row) acc
        (>= col max-col) (recur acc (inc row)
                                (- c (- radius (abs (- (inc row) r))))
                                (inc (+ c (- radius (abs (- r (inc row)))))))

        (not (and (>= row 0) (< row h) (>= col 0) (< col w)))
        (recur acc row (inc col) max-col)
        
        :else
        (recur (f acc row col) row (inc col) max-col)))))

(def MAX-COL 1000)
(def MAX-ROW 1000)

(s/sdefn hash-vec2 [^vec2 {:fields [row col]}] ^long
  (+ col (* row MAX-COL)))

(defn hash-ints [^long row ^long col] ^long
  (+ col (* row MAX-COL)))

(defn ht-set [^longs ht ^vec2 p ^long val]
  (aset-long ht (hash-vec2 p) val)
  ht)

(defn ht-getp [^longs ht ^vec2 p] ^long
  (aget ht (hash-vec2 p)))

(defn ht-get [^longs ht ^long row ^long col] ^long
  (aget ht (hash-ints row col)))

(defn num-cheats-from [^Long min-savings ^Long max-cheat-time ^Long height ^Long width
                       ^longs path-lens ^vec2 cur]
  (s/with-slots [{:fields [row col]} ^vec2 cur
                 r row c col
                 cur-path-len (ht-get path-lens r c)]
    (fold-circle-coords
      (fn [acc row col]
        (let [dst-path-len (ht-get path-lens row col)]
          (if (== -1 dst-path-len)
            acc
            (let [new-path-len (+ (abs (- r row)) (abs (- c col)))
                  length-diff (- cur-path-len dst-path-len
                                 new-path-len)]
              (if (< length-diff min-savings)
                acc
                (inc acc))))))
      0
      height
      width
      max-cheat-time
      cur)))

(defn part-with-params [min-savings max-cheat-time s]
  (let [{:keys [^vec2 start ^vec2 end grid]} (parse-input s)
        path (-> (search/bfs (partial plain-neighbors grid)
                             start
                             #(= end ^vec2 %))
                 :prevs
                 (trace-path end start))
        h (g/height grid)
        w (g/width grid)
        ;path-lens (zipmap path (range))
        path-lens ^longs (reduce (fn [ht [coord dist]] (ht-set ht coord dist))
                                 (long-array (* MAX-ROW MAX-COL) -1)
                                 (map vector path (range)))
        ]
    (->> path
         (map #(num-cheats-from min-savings max-cheat-time h w path-lens %))
         (reduce +))))

(def part-1 (partial part-with-params 100 2))
(def part-2 (partial part-with-params 100 20))

(comment
  (def lines (str/split-lines (slurp "inputs/day20.test")))
  (def inp-lines (str/split-lines (slurp "inputs/day20.inp")))

  ;(part-with-params 0 2 lines) ; 44
  (+ 32 31 29 39 25 23 20 19 12 14 12 22 4 3) ; 285
  (part-with-params 50 20 lines) ; 285

  (time (part-1 inp-lines)) ; 1365 ~ 140 ms ~ 90 ms

  (time (part-2 inp-lines)) ; 986082 ~ 2210 ms ~ 1560 ms

  )

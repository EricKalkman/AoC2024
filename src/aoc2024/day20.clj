(ns aoc2024.day20
  (:require [aoc2024.searches :as search]
            [aoc2024.grids :as g]
            [clojure.string :as str]))

(defn parse-input [lines]
  (let [grid (->> lines g/parse-grid)
        tiles (->> (g/coords grid) (group-by #(g/grid-get grid %)))]
    {:grid grid
     :walls (into #{} (tiles \#))
     :start (first (tiles \S))
     :end (first (tiles \E))}))

(defn plain-neighbors [walls coord]
  ; don't need to check if in grid because surrounded by walls
  (->> (g/neighbors4 coord)
       (filterv (complement walls))))

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
                   (inc (+ c (- radius (abs (- r row))))))
        ;:when (and (<= 0 row (dec height))
        ;           (<= 0 col (dec width)))
        ]
    [row col]))

(def MAX-COL 1000)
(def MAX-ROW 1000)

(defn hash-coord [[row col]] (+ col (* MAX-COL row)))

(defn cheat-from [max-cheat-time path-lens cur]
  (let [cur-path-len (path-lens (hash-coord cur))]
    (->> (circle-coords cur max-cheat-time)
         (keep #(if-let [dst-path-len (path-lens (hash-coord %))]
                  (let [new-path-len (manh-dist cur %)
                        length-diff (- cur-path-len dst-path-len
                                       new-path-len)]
                    (if (<= length-diff 0)
                      nil
                      length-diff))
                  nil)))))

(defn part-with-params [min-savings max-cheat-time s]
  (let [{:keys [start end walls]} (parse-input s)
        path (-> (search/bfs (partial plain-neighbors walls)
                             start
                             #(= % end))
                  :prevs
                  (trace-path end start))
        path-lens (zipmap (map hash-coord path) (range))]
    (->> path
         (transduce
           (comp
             (mapcat #(cheat-from max-cheat-time path-lens %))
             (map #(if (>= % min-savings) 1 0)))
           + 0))))

(def part-1 (partial part-with-params 100 2))
(def part-2 (partial part-with-params 100 20))

(comment
  (def lines (str/split-lines (slurp "inputs/day20.test")))
  (def inp-lines (str/split-lines (slurp "inputs/day20.inp")))

  (part-with-params 0 2 lines) ; 44
  (+ 32 31 29 39 25 23 20 19 12 14 12 22 4 3) ; 285
  (part-with-params 50 20 lines) ; 285

  (part-1 inp-lines) ; 1365

  (time (part-2 inp-lines)) ; 986082

  )

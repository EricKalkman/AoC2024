(ns aoc2024.day16
  (:require [clojure.string :as str]
            [aoc2024.grids :as g]
            [aoc2024.searches :as search]))

(defrecord node [pos dir]
  Comparable
  (compareTo [this other]
    (let [c (compare (.pos this) (.pos other))]
      (case c
        0 (compare (.dir this) (.dir other))
        c))))

(defn parse-input [s]
  (let [grid (g/parse-grid (str/split-lines (str/trim s)))
        {spaces \. start \S end \E}
        (->> (g/coords grid) (group-by #(g/grid-get grid %)))]
    {:start (first start)
     :end (first end)
     :spaces (into #{(first start) (first end)} spaces)}))

(def TURN-COST 1000)
(def MOVE-COST 1)

(defn neighbors [spaces end ^node n]
  (let [pos (.pos n)
        dir (.dir n)]
    (if (= end pos)
      []
      (->> [[(->node (g/move dir 1 pos) dir) MOVE-COST]
            [(->node pos (g/turn-left dir)) TURN-COST]
            [(->node pos (g/turn-right dir)) TURN-COST]]
           (filterv #(spaces (.pos ^node (first %))))))))

(defn part-1 [s]
  (let [{:keys [spaces start end]} (parse-input s)]
    (->> (search/dijkstra (partial neighbors spaces end)
                          [(->node start :right)]
                          #(= end (.pos %)))
         :cost)))

(defn part-2 [s]
  (let [{:keys [spaces start end]} (parse-input s)
        {:keys [prevs lasts]}
        (search/dijkstra (partial neighbors spaces end)
                         [(->node start :right)]
                         #(= end (.pos %)))]
    (->> lasts
         (mapcat (comp
                   #(map :pos %)
                   keys
                   :prevs
                   #(search/bfs (comp (fn [x] (map second x)) prevs) % (constantly false))))
         (into #{})
         count)))

(comment
  (def test-inp "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
")
  (def test-inp2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

  (def inp (parse-input test-inp))
  (neighbors (:spaces inp) (:end inp) (->node (:start inp) :right))
; [[{:pos [13 2], :dir :right} 1]
;  [{:pos [13 1], :dir :up} 1000]
;  [{:pos [13 1], :dir :down} 1000]]

  (part-1 test-inp) ; 7036
  (part-1 test-inp2) ; 11048
  (part-1 (slurp "inputs/day16.inp")) ; 95476

  (part-2 test-inp) ; 45
  (part-2 test-inp2) ; 64
  (part-2 (slurp "inputs/day16.inp")) ; 511

  )

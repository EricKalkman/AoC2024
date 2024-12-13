(ns aoc2024.day12
  (:require [aoc2024.grids :as g]
            [aoc2024.searches :as search]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn neighbors [grid coord]
  (let [color (g/grid-get grid coord)]
    (->> (g/neighbors4 coord grid)
         (filter #(= color (g/grid-get grid %))))))

(defn num-convex-corners [dirs]
  ; convex corners
  (cond
    ; if cur has 4 dirs facing different plant types, then it is an isolate and has 4 corners
    (= 4 (count dirs)) 4
    ; if there are more than two edges facing differen plant types, then we may have at
    ; least one corner
    (>= (count dirs) 2)
    (->> [#{:up :right} #{:up :left} #{:down :right} #{:down :left}]
         (filter #(set/subset? % dirs))
         count)
    ; otherwise, we're interior or on an edge in between corners
    :else 0))

(defn num-concave-corners [grid dirs cur]
  ;concave corners
  (let [color (g/grid-get grid cur)
        inside-dirs (set/difference #{:up :right :down :left} dirs)]
    ; count the number of cells that are adjacent in such a way that one of the corners
    ; of the current cell is a concave corner of the plot (i.e., the number of ways that the
    ; current cell can form an L shape with adjacent cells)
    (->> [#{:up :right} #{:right :down} #{:down :left} #{:left :up}]
         (filter #(and (set/subset? % inside-dirs)
                       ; the cup of the L must be outside the current plot
                       (not= color (->> cur (g/move (first %) 1) (g/move (second %) 1)
                                        (g/grid-get grid)))))
         count)))

(defn border-property-visit [grid perimeter n-corners]
  (fn
    ([] {:perimeter perimeter :n-corners n-corners})
    ([_prevs cur]
     (let [color (g/grid-get grid cur)
           ; all directions from current cell that lead to plots with other plants
           ds (->> g/DIRS4
                   (into #{} (filter #(not= color (g/grid-get grid (g/move % 1 cur))))))]
       (border-property-visit
         grid
         (+ perimeter (count ds))
         (+ n-corners
            (num-convex-corners ds)
            (num-concave-corners grid ds cur)))))))

(defn find-plots [grid]
  (loop [coords (into #{} (g/coords grid))
         plots []]
    (if (empty? coords)
      plots
      (let [{:keys [prevs visit]}
            (search/bfs (partial neighbors grid) (first coords) (constantly false)
                        (border-property-visit grid 0 0))
            visited (into #{} (keys prevs))
            {:keys [perimeter n-corners]} (visit)]
        (recur (set/difference coords visited)
               (conj plots {:type (g/grid-get grid (first coords))
                            :coords visited
                            :perimeter perimeter
                            :n-corners n-corners}))))))

(defn part-1 [lines]
  (let [grid (g/parse-grid lines)]
    (->> grid
         find-plots
         (reduce #(+ %1 (* (:perimeter %2)
                           (count (:coords %2))))
                 0))))

(defn part-2 [lines]
  (let [grid (g/parse-grid lines)]
    (->> grid
         find-plots
         (map #(* (count (:coords %)) (:n-corners %)))
         (reduce +))))

(comment

  (def test-inp
"AAAA
BBCD
BBCC
EEEC")

  (def test-inp2 "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")

  (def test-inp3
".....
.aaa.
.aa..
.aaa.
.....")

  (def test-inp4
"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

  (def g1 (g/parse-grid (str/split-lines test-inp)))
  (def g2 (g/parse-grid (str/split-lines test-inp2)))
  (def g3 (g/parse-grid (str/split-lines test-inp3)))
  (def g4 (g/parse-grid (str/split-lines test-inp4)))

  (find-plots g1)
  (part-1 (str/split-lines test-inp4)) ; 1930
  (part-1 (str/split-lines (slurp "inputs/day12.inp"))) ; 1446042

  (part-2 (str/split-lines test-inp4)) ; 1206
  (part-2 (str/split-lines (slurp "inputs/day12.inp"))) ; 902742

  )

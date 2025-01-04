(ns aoc2024.day12
  (:require [aoc2024.grids :as g]
            [aoc2024.searches :as search]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [aoc2024.grids vec2 Grid]))

(defn neighbors [^Grid grid ^vec2 coord]
  (let [color ^char (grid coord)]
    (->> (g/neighbors4 coord)
         (filterv #(and (contains? grid %) (= color (grid ^vec2 %)))))))

(defn num-convex-corners [dirs]
  (->> [#{:up :right} #{:up :left} #{:down :right} #{:down :left}]
       (filter #(set/subset? % dirs))
       count))

(defn num-concave-corners [^Grid grid dirs ^vec2 cur]
  ;concave corners
  (let [color ^char (grid cur)
        inside-dirs (set/difference #{:up :right :down :left} dirs)]
    ; count the number of cells that are adjacent in such a way that one of the corners
    ; of the current cell is a concave corner of the plot (i.e., the number of ways that the
    ; current cell can form an L shape with adjacent cells)
    (->> [#{:up :right} #{:right :down} #{:down :left} #{:left :up}]
         (filter #(and (set/subset? % inside-dirs)
                       ; the cup of the L must be outside the current plot
                       (not= color
                             (let [pos (->> cur (g/move (first %) 1) (g/move (second %) 1))]
                               (grid pos nil)))))
         count)))

(defn border-property-visit [^Grid grid ^long perimeter ^long n-corners]
  (fn
    ([] {:perimeter perimeter :n-corners n-corners})
    ([_prevs ^vec2 cur]
     (let [color ^char (grid cur)
           ; all directions from current cell that lead to plots with other plants
           ds (->> g/DIRS4
                   (into #{} (filter #(not= color (grid (g/move % 1 cur) nil)))))]
       (border-property-visit
         grid
         (+ perimeter (count ds))
         (+ n-corners
            (num-convex-corners ds)
            (num-concave-corners grid ds cur)))))))

(defn find-plots [^Grid grid]
  (loop [coords (persistent! (g/fold-grid-keys conj! (transient #{}) grid))
         plots []]
    (if (empty? coords)
      plots
      (let [{:keys [prevs visit]}
            (search/bfs (partial neighbors grid) (first coords) (constantly false)
                        (border-property-visit grid 0 0))
            visited (into #{} (keys prevs))
            {:keys [perimeter n-corners]} (visit)]
        (recur (set/difference coords visited)
               (conj plots {:type (grid (first coords))
                            :coords visited
                            :perimeter perimeter
                            :n-corners n-corners}))))))

(defn part-1 [lines]
  (let [grid (g/lines->grid lines)]
    (->> grid
         find-plots
         (reduce #(+ %1 (* (:perimeter %2)
                           (count (:coords %2))))
                 0))))

(defn part-2 [lines]
  (let [grid (g/lines->grid lines)]
    (->> grid
         find-plots
         (map #(* (count (:coords %)) (:n-corners %)))
         (reduce +))))

(comment
  (def test-inp (str/split-lines (slurp "inputs/day12.test")))
  (def test-g (g/lines->grid test-inp))

  (time (part-1 (str/split-lines (slurp "inputs/day12.inp")))) ; 1446042 ~ 220 ms

  (time (part-2 (str/split-lines (slurp "inputs/day12.inp")))) ; 902742 ~ 220 ms

  )

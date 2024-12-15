(ns aoc2024.day15
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [aoc2024.searches :as search]))

(defn parse-input [s]
  (let [[grid-str commands] (str/split s #"\n\n")
        grid (g/parse-grid (map vec (str/split-lines grid-str)))
        tiles (group-by #(g/grid-get grid %) (g/coords grid))]
    {:width (g/width grid)
     :height (g/height grid)
     :robot (first (tiles \@))
     :walls (into #{} (tiles \#))
     :boxes (into #{} (tiles \O))
     :commands (str/join (str/split-lines commands))}))

(defn pushable-blocks [boxes walls coord dir]
  (let [blocks (->> (iterate #(g/move dir 1 %) coord)
                    rest
                    (transduce (take-while boxes) conj))]
    (cond
      (and (empty? blocks) (walls (g/move dir 1 coord))) nil
      (empty? blocks) []
      (walls (g/move dir 1 (.nth blocks (dec (count blocks))))) nil
      :else blocks)))

(def dirmap {\> :right \< :left \^ :up \v :down})
(def dir-to-char {:right \> :left \< :up \^ :down \v})

(defn print-step [cur dir width height walls boxes]
  (doseq [row (range height)]
    (doseq [col (range width)]
      (print
        (cond
          (= cur [row col]) (dir-to-char dir)
          (walls [row col]) \#
          (boxes [row col]) \O
          :else \.)))
    (println ""))
  (println ""))

(defn simulate [{:keys [walls boxes robot commands]}]
  (loop [robot robot
         commands commands
         boxes boxes]
    (if-let [dir (dirmap (first commands))]
      (do
        ;(print-step robot dir width height walls boxes)
        ;(println)
        (if-let [pushable (pushable-blocks boxes walls robot dir)]
          (recur (g/move dir 1 robot)
                 (rest commands)
                 (as-> boxes $
                   (reduce disj $ pushable)
                   (into $ (map #(g/move dir 1 %) pushable))))
          (recur robot (rest commands) boxes)))
      {:robot robot :boxes boxes})))

(defn gps-coordinate [[row col]] (+ col (* row 100)))

(defn part-1 [s]
  (->> s
       str/trim
       parse-input
       simulate
       :boxes
       (map gps-coordinate)
       (reduce +)))

(defn double-grid-cell [[row col]]
  [[row (* 2 col)] [row (inc (* 2 col))]])

(defn make-coords-to-boxes-map [boxes]
  (->> (for [box boxes
             coord box]
         [coord box])
       (into {})))

(defn stretch-map [{:keys [width height walls boxes robot commands]}]
  {:width (* 2 width)
   :height height
   :walls (->> walls (mapcat double-grid-cell) (into #{}))
   :box-coords (->> boxes
                    (map double-grid-cell)
                    make-coords-to-boxes-map)
   :robot [(first robot) (* 2 (second robot))]
   :commands commands})

(defn neighbors [box-coords dir cur]
  (let [next-coord (g/move dir 1 cur)]
    (->> (or (box-coords next-coord) [])
         (concat (box-coords cur)))))

(defn move-box [dir box]
  (mapv #(g/move dir 1 %) box))

(defn simulate-part-2 [{:keys [walls box-coords robot commands]}]
  (loop [robot robot
         commands commands
         box-coords box-coords]
    (if-let [dir (dirmap (first commands))]
      (let [next-coord (g/move dir 1 robot)]
        (cond
          (walls next-coord) (recur robot (rest commands) box-coords)
          (not (box-coords next-coord)) (recur next-coord (rest commands) box-coords)
          :else
          (let [coords-to-push (->> (search/bfs (partial neighbors box-coords dir)
                                                next-coord (constantly false))
                                    :prevs
                                    keys)
                boxes-to-push (into #{} (map box-coords) coords-to-push)]
            (if (some #(walls (g/move dir 1 %)) coords-to-push)
              (recur robot (rest commands) box-coords)
              (recur next-coord
                     (rest commands)
                     (as-> box-coords $
                       (reduce dissoc $ coords-to-push)
                       (merge $ (->> boxes-to-push (map #(move-box dir %)) make-coords-to-boxes-map))))))))
      {:robot robot :box-coords box-coords})))

(defn part-2 [s]
  (->> s
       str/trim
       parse-input
       stretch-map
       simulate-part-2
       :box-coords
       vals
       (into #{})
       (map (comp gps-coordinate first))
       (reduce +)))

(comment
  (def test-inp
"########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

  (def test-inp2 "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

  (let [inp (parse-input test-inp2)]
    (->> (simulate inp)
         :boxes
         (sort)))

  (part-1 test-inp) ; 2028
  (part-1 test-inp2) ; 10092
  (part-1 (slurp "inputs/day15.inp")) ; 1412971

  (def test-inp3 "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

  (let [inp (parse-input test-inp2)
        expanded (stretch-map inp)
        ]
    (->> expanded
         simulate-part-2
         :box-coords
         vals
         (into #{})
         (map (comp gps-coordinate first))
         (reduce +)))

  (part-2 test-inp) ; 1751
  (part-2 test-inp2) ; 9021
  (part-2 test-inp3) ; 618
  (part-2 (slurp "inputs/day15.inp")) ; 1429299
  )

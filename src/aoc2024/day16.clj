(ns aoc2024.day16
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [clojure.string :as str]
            [structural.core :as s]
            [aoc2024.grids :as g]
            [aoc2024.searches :as search])
  (:import [aoc2024.grids vec2 Grid]
           [clojure.lang Keyword IPersistentVector]))

(defrecord node [^vec2 pos ^Keyword dir]
  Comparable
  (compareTo [_this other]
    (let [c (compare pos (.pos ^node other))]
      (case c
        0 (compare dir (.dir ^node other))
        c))))

(defn parse-input [s]
  (let [grid (g/lines->grid (str/split-lines (str/trim s)))
        start (g/first-coord-of grid #(= \S %))
        end (g/first-coord-of grid #(= \E %)) ]
    {:start start
     :end end
     :grid (-> grid
               (assoc start \.)
               (assoc end \.))}))

(def TURN-COST 1000)
(def MOVE-COST 1)

(defn neighbors [^Grid grid ^vec2 end ^node n]
  (s/with-slots [{:fields [pos dir]} ^node n] 
    (if (= end pos)
      []
      (->> [[(->node (g/move dir 1 pos) dir) MOVE-COST]
            [(->node (g/move (g/turn-left dir) 1 pos) (g/turn-left dir))
             (+ MOVE-COST TURN-COST)]
            [(->node (g/move (g/turn-right dir) 1 pos) (g/turn-right dir))
             (+ MOVE-COST TURN-COST)]]
           (filterv #(let [pos (.pos ^node (first ^IPersistentVector %))]
                       (= \. ^char (grid pos nil))))))))

(defn part-1 [s]
  (let [{:keys [^Grid grid ^vec2 start ^vec2 end]} (parse-input s)]
    (->> (search/dijkstra (partial neighbors grid end)
                          [(->node start :right)]
                          #(= end (.pos ^node %)))
         :cost)))

(defn part-2 [s]
  (let [{:keys [^Grid grid ^vec2 start ^vec2 end]} (parse-input s)
        {:keys [prevs lasts]}
        (search/dijkstra (partial neighbors grid end)
                         [(->node start :right)]
                         #(= end (.pos ^node %)))]
    (->> lasts
         (mapcat (comp
                   #(map :pos %)
                   keys
                   :prevs
                   #(search/bfs (comp :nodes prevs) ^vec2 % (constantly false))))
         (into #{})
         count)))

(comment
  (time (part-1 (slurp "inputs/day16.inp"))) ; 95476 ~ 120 ms
  (time (part-2 (slurp "inputs/day16.inp"))) ; 511 ~ 175 ms
  )

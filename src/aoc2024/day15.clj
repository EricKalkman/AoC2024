(ns aoc2024.day15
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [structural.core :as s]
            [aoc2024.searches :as search])
  (:import [aoc2024.grids vec2]
           [clojure.lang PersistentVector]))

(defn parse-input [s]
  (let [[grid-str commands] (str/split s #"\n\n")
        grid (g/lines->grid (str/split-lines grid-str))
        tiles (g/fold-grid-kv #(update %1 %3 (fn [x] (conj (or x #{}) %2))) {} grid)]
    {:width (g/width grid)
     :height (g/height grid)
     :robot (first (tiles \@))
     :walls (tiles \#)
     :boxes (tiles \O)
     :commands (str/join (str/split-lines commands))}))

(defn pushable-blocks [boxes walls ^vec2 coord dir]
  (let [blocks
        ^PersistentVector
        (->> (iterate #(g/move dir 1 %) coord)
             rest
             (transduce (take-while boxes) conj))]
    (cond
      (and (empty? blocks) (walls (g/move dir 1 coord))) nil
      (empty? blocks) []
      (walls (g/move dir 1 (.nth ^PersistentVector blocks (dec (count blocks))))) nil
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

(defn simulate [{:keys [walls boxes ^vec2 robot commands]}]
  (loop [robot robot
         commands (seq commands)
         boxes boxes]
    (if-let [dir (dirmap (first commands))]
      (do
        ;(print-step robot dir width height walls boxes)
        ;(println)
        (if-let [pushable (pushable-blocks boxes walls ^vec2 robot dir)]
          (recur (g/move dir 1 robot)
                 (rest commands)
                 (as-> boxes $
                   (reduce disj $ pushable)
                   (into $ (map #(g/move dir 1 %) pushable))))
          (recur robot (rest commands) boxes)))
      {:robot robot :boxes boxes})))

(s/sdefn gps-coordinate [^vec2 {:fields [row col]}]
  (+ col (* row 100)))

(defn part-1 [s]
  (->> s
       str/trim
       parse-input
       simulate
       :boxes
       (map gps-coordinate)
       (reduce +)))

(s/sdefn double-grid-cell [^vec2 {:fields [row col]}]
  [(g/->vec2 row (* 2 col)) (g/->vec2 row (inc (* 2 col)))])

(defn make-coords-to-boxes-map [boxes]
  (->> (for [box boxes
             ^vec2 coord box]
         [coord box])
       (into {})))

(defn stretch-map [{:keys [^long width ^long height walls boxes ^vec2 robot commands]}]
  {:width (* 2 width)
   :height height
   :walls (->> walls (mapcat double-grid-cell) (into #{}))
   :box-coords (->> boxes
                    (map double-grid-cell)
                    make-coords-to-boxes-map)
   :robot (g/->vec2 (.row robot) (* 2 (.col robot)))
   :commands commands})

(defn neighbors [box-coords dir ^vec2 cur]
  (let [next-coord (g/move dir 1 cur)]
    (->> (or (box-coords next-coord) [])
         (concat (box-coords cur)))))

(defn move-box [dir box]
  (mapv #(g/move dir 1 %) box))

(defn simulate-part-2 [{:keys [walls box-coords robot commands]}]
  (loop [robot ^vec2 robot
         commands (seq commands)
         box-coords box-coords]
    (if-let [dir (dirmap (first commands))]
      (let [next-coord (g/move dir 1 robot)]
        (cond
          ; if we would run into a wall
          (walls next-coord) (recur robot (rest commands) box-coords)
          ; if we are not adjacent to a box, simply move
          (not (box-coords next-coord)) (recur next-coord (rest commands) box-coords)
          :else
          (let [coords-to-push (->> (search/bfs (partial neighbors box-coords dir)
                                                next-coord (constantly false))
                                    :prevs
                                    keys) ; all coords in line along dir corresponding to boxes
                ; the boxes themselves
                boxes-to-push (into #{} (map box-coords) coords-to-push)]
            ; if one of the boxes is blocked by a wall
            (if (some #(walls (g/move dir 1 %)) coords-to-push)
              ; do nothing
              (recur robot (rest commands) box-coords)
              ; advance the robot and update the coord->boxes map
              (recur next-coord
                     (rest commands)
                     (as-> box-coords $
                       ; remove the boxes to be moved
                       (reduce dissoc $ coords-to-push)
                       ; re-add the boxes after moving them
                       (merge $ (->> boxes-to-push (map #(move-box dir %)) make-coords-to-boxes-map))))))))
      {:robot robot :box-coords box-coords})))

(defn part-2 [s]
  (->> s
       str/trim
       parse-input
       stretch-map
       simulate-part-2
       :box-coords
       vals       ; get boxes
       (into #{}) ; deduplicate boxes
       (map (comp gps-coordinate first)) ; use first (leftmost) coordinate to determine gps
       (reduce +)))

(comment
  (time (part-1 (slurp "inputs/day15.inp"))) ; 1412971
  (time (part-2 (slurp "inputs/day15.inp"))) ; 1429299
  )

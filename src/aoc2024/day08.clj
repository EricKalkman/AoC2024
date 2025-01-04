(ns aoc2024.day08
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as m])
  (:import [aoc2024.grids vec2 Grid]))

(defn collect-antennae [^Grid grid]
  (->> (g/all-coords-of grid #(not= \. ^char %))
       (group-by grid)))

(defn coords-in-line
  "Calculates all points in grid with integer coordinates on a line from a to b starting from a."
  [^Grid grid ^vec2 a ^vec2 b]
  (let [[dr dc] (g/vec2- b a)
        denom (if (or (zero? dr) (zero? dc)) (max-key abs dr dc) (m/gcd (abs dr) (abs dc)))
        delta (g/->vec2 (/ dr denom) (/ dc denom))]
    (->> a
         (iterate (fn [^vec2 coord] (g/vec2+ coord delta)))
         (take-while #(contains? grid %)))))

(defn first-double-dist [^vec2 a ^vec2 b coords]
  (let [[dr dc] (g/vec2- b a)
        denom (if (or (zero? dr) (zero? dc)) (max-key abs dr dc) (m/gcd (abs dr) (abs dc)))]
    (nth coords (* 2 denom) nil)))

(defn combinations
  "Returns all n_C_2 combinations of as."
  [as]
  (->> as
       (iterate rest)
       (take-while #(seq (rest %))) ; while > 1 element
       (mapcat #(map (partial vector (first %)) (rest %)))))

(defn antinodes-of [which ^Grid grid ^vec2 a ^vec2 b]
  ; only take the first frequency (-> antinode is twice the distance from a
  ; as b and vice versa)
  (->>
    (case which
      :first
      (->> [[a b] [b a]]
           (keep (fn [[^vec2 x ^vec2 y]] (first-double-dist x y (coords-in-line grid x y)))))
      :all
      (concat (coords-in-line grid a b) (coords-in-line grid b a)))
    (into #{})))

(defn all-antinodes [which ^Grid grid as]
  (->> (combinations as)
       (map #(apply antinodes-of which grid %))
       (reduce set/union #{})))

(defn part [which lines]
  (let [grid ^Grid (g/lines->grid lines)
        ants (collect-antennae grid)]
    (->> (vals ants)
         (map #(all-antinodes which grid %))
         (reduce set/union #{})
         count)))

(def part-1 (partial part :first))
(def part-2 (partial part :all))

(comment
  (def test-lines (str/split-lines (slurp "inputs/day08.test")))

  (part-1 (str/split-lines (slurp "inputs/day08.inp"))) ; 329
  (part-2 (str/split-lines (slurp "inputs/day08.inp"))) ; 1190
)

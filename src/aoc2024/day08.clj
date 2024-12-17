(ns aoc2024.day08
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as m]))

(defn collect-antennae [grid]
  (->> (g/coords grid)
       (filter #(not= \. (g/grid-get grid %)))
       (group-by #(g/grid-get grid %))))

(defn coords-in-line
  "Calculates all points in grid with integer coordinates on a line from a to b starting from a."
  [grid a b]
  (let [[dr dc] (g/coord- b a)
        denom (if (or (zero? dr) (zero? dc)) (max-key abs dr dc) (m/gcd (abs dr) (abs dc)))
        delta [(/ dr denom) (/ dc denom)]]
    (->> a
         (iterate (fn [coord] (g/coord+ coord delta)))
         (take-while #(g/in-grid? grid %)))))

(defn first-double-dist [a b coords]
  (let [[dr dc] (g/coord- b a)
        denom (if (or (zero? dr) (zero? dc)) (max-key abs dr dc) (m/gcd (abs dr) (abs dc)))]
    (nth coords (* 2 denom) nil)))

(defn combinations
  "Returns all n_C_2 combinations of as."
  [as]
  (->> as
       (iterate rest)
       (take-while #(seq (rest %))) ; while > 1 element
       (mapcat #(map (partial vector (first %)) (rest %)))))

(defn antinodes-of [which grid a b]
  ; only take the first frequency (-> antinode is twice the distance from a
  ; as b and vice versa)
  (->>
    (case which
      :first
      (->> [[a b] [b a]]
           (keep (fn [[x y]] (first-double-dist x y (coords-in-line grid x y)))))
      :all
      (concat (coords-in-line grid a b) (coords-in-line grid b a)))
    (into #{})))

(defn all-antinodes [which grid as]
  (->> (combinations as)
       (map #(apply antinodes-of which grid %))
       (reduce set/union #{})))

(defn part [which lines]
  (let [grid (g/parse-grid lines)
        ants (collect-antennae grid)]
    (->> (vals ants)
         (map #(all-antinodes which grid %))
         (reduce set/union #{})
         count)))

(def part-1 (partial part :first))
(def part-2 (partial part :all))

(comment
  (part-1 (str/split-lines (slurp "inputs/day08.inp"))) ; 329
  (part-2 (str/split-lines (slurp "inputs/day08.inp"))) ; 1190
)

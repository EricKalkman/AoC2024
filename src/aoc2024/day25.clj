(ns aoc2024.day25
  (:require [clojure.string :as str]
            [aoc2024.grids :as g])
  (:import [aoc2024.grids vec2]))

(defn inp-kind [grid]
  (if (every? #(= \# (grid (g/->vec2 0 %))) (range (g/width grid)))
    :lock
    :key))

(defn parse-input [s]
  (->> (str/split s #"\n\n")
       (map #(g/lines->grid (str/split % #"\n")))
       (group-by inp-kind)))

(defn heights [chooser extremum grid]
  (->> (g/all-coords-of grid #(= % \#))
       (reduce
         (fn [h ^vec2 point]
           (update h (.col point) #(chooser (or % extremum) (.row point))))
         (sorted-map))))

(def MAX-HEIGHT 7)

(defn part-1 [s]
  (let [{keys :key locks :lock}
        (as-> s $
          (str/trim s)
          (parse-input $)
          (update $ :lock #(map (comp (fn [x] (update-vals x inc))
                                      (partial heights max 0))
                                %))
          (update $ :key #(map (comp (fn [x] (update-vals x (fn [y] (- MAX-HEIGHT y))))
                                     (partial heights min Long/MAX_VALUE))
                               %)))]
    (->> (for [lock locks
               key keys
               :when (->> (map + (vals key) (vals lock))
                          (every? #(<= % MAX-HEIGHT)))]
           1)
         count)))

(comment
  (def test-inp (str/trim (slurp "inputs/day25.test")))
  (def inp (str/trim (slurp "inputs/day25.inp")))

  (part-1 test-inp) ; 3
  (time (part-1 inp)) ; 3090
  )

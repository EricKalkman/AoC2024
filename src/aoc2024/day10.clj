(ns aoc2024.day10
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [aoc2024.searches :as search])
  (:import [aoc2024.grids Grid vec2]))

(defn parse-input [lines] ^Grid
  ; note: Character/digit returns -1 for non-digit characters, which is fine here
  ; given the monotonicity of edges
  (g/lines->grid (map #(map (fn [^Character c] (Character/digit c 10)) %) lines)))

(defn neighbors [^Grid grid ^vec2 pos]
  (let [cur-height ^long (grid pos)]
    (if (= 9 cur-height)
      []
      (->> (g/neighbors4 pos)
           (filterv #(and (contains? grid ^vec2 %)
                          (== (inc cur-height) ^long (grid ^vec2 %))))))))

(defn trail-score [^Grid grid neighfunc pos-indexer ^vec2 start]
  (->> (search/bfs neighfunc start (constantly false))
       :prevs
       keys
       (filter #(= 9 (grid ^vec2 (pos-indexer ^vec2 %))))
       count))

(defn part-1 [lines]
  (let [grid (parse-input lines)]
    (->> (g/all-coords-of grid zero?)
         (map (partial trail-score grid (partial neighbors grid) identity))
         (reduce +))))

(defn part-2 [lines]
  (let [grid (parse-input lines)]
    (->> (g/all-coords-of grid zero?)
         (map (comp (partial trail-score grid
                             (comp #(mapv (fn [^vec2 neigh] [^vec2 neigh (gensym)]) %)
                                   (partial neighbors grid)
                                   first)
                             first)
                    #(vector % (gensym))))
         (reduce +))))

(defn visit-smarter [neighfunc coords]
  (fn
    ([] coords)
    ([_ ^vec2 cur]
     (let [cur-count ^long (coords cur)]
       (visit-smarter
         neighfunc
         (reduce #(update %1 %2 (fn [c] (+ ^long (or c 0) cur-count)))
                 coords
                 ^long (neighfunc cur)))))))

(defn call [f] (f))

(defn trail-rating-smarter [^Grid grid ^vec2 start]
  (let [counts (->> (search/bfs (partial neighbors grid) start (constantly false)
                                (visit-smarter (partial neighbors grid) {start 1}))
                    :visit
                    call)]
    (->> counts
         keys
         (filter #(= 9 (grid ^vec2 %)))
         (map counts)
         (reduce +))))

(defn part-2-smarter [lines]
  (let [grid (parse-input lines)]
    (->> (g/all-coords-of grid zero?)
         (map (partial trail-rating-smarter grid))
         (reduce +))))

(comment
  (time (part-1 (str/split-lines (slurp "inputs/day10.inp")))) ; 646
  (time (part-2 (str/split-lines (slurp "inputs/day10.inp")))) ; 1494
  (time (part-2-smarter (str/split-lines (slurp "inputs/day10.inp")))) ; 1494

  (def test-inp-boojum "9999999999999999999
9999999998999999999
9999999987899999999
9999999876789999999
9999998765678999999
9999987654567899999
9999876543456789999
9998765432345678999
9987654321234567899
9876543210123456789
9987654321234567899
9998765432345678999
9999876543456789999
9999987654567899999
9999998765678999999
9999999876789999999
9999999987899999999
9999999998999999999
9999999999999999999")
  (def bj-lines (str/split-lines test-inp-boojum))
  (def bj-g (parse-input bj-lines))

  (time (part-2 (str/split-lines test-inp-boojum))) ; 2044
  (time (part-2-smarter (str/split-lines test-inp-boojum))) ; 2044
  )

(ns aoc2024.day10
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]
            [aoc2024.searches :as search]))

(defn parse-input [lines]
  ; note: Character/digit returns -1 for non-digit characters, which is fine here
  ; given the monotonicity of edges
  (mapv #(mapv (fn [c] (Character/digit c 10)) %) lines))

(defn neighbors [grid pos]
  (let [cur-height (g/grid-get grid pos)]
    (if (= 9 cur-height)
      []
      (->> (g/neighbors4 pos grid)
           (filterv #(= (inc cur-height) (g/grid-get grid %)))))))

(defn trail-score [grid neighfunc pos-indexer start]
  (->> (search/bfs neighfunc start (constantly false))
       :prevs
       keys
       (filter #(= 9 (g/grid-get grid (pos-indexer %))))
       count))

(defn part-1 [lines]
  (let [grid (parse-input lines)]
    (->> (g/coords grid)
         (filter #(zero? (g/grid-get grid %)))
         (map (partial trail-score grid (partial neighbors grid) identity))
         (reduce +))))

(defn part-2 [lines]
  (let [grid (parse-input lines)]
    (->> (g/coords grid)
         (filter #(zero? (g/grid-get grid %)))
         (map (comp (partial trail-score grid
                             (comp #(mapv (fn [neigh] [neigh (gensym)]) %)
                                   (partial neighbors grid)
                                   first)
                             first)
                    #(vector % (gensym))))
         (reduce +))))

(defn visit-smarter [neighfunc coords]
  (fn
    ([] coords)
    ([_ cur]
     (let [cur-count (coords cur)]
       (visit-smarter
         neighfunc
         (reduce #(update %1 %2 (fn [c] (+ (or c 0) cur-count)))
                 coords
                 (neighfunc cur)))))))

(defn call [f] (f))

(defn trail-rating-smarter [grid start]
  (let [counts (->> (search/bfs (partial neighbors grid) start (constantly false)
                                (visit-smarter (partial neighbors grid) {start 1}))
                    :visit
                    call)]
    (->> counts
         keys
         (filter #(= 9 (g/grid-get grid %)))
         (map counts)
         (reduce +))))

(defn part-2-smarter [lines]
  (let [grid (parse-input lines)]
    (->> (g/coords grid)
         (filter #(zero? (g/grid-get grid %)))
         (map (partial trail-rating-smarter grid))
         (reduce +))))

(comment
  (part-1 (str/split-lines (slurp "inputs/day10.inp"))) ; 646
  (part-2 (str/split-lines (slurp "inputs/day10.inp"))) ; 1494
  (part-2-smarter (str/split-lines (slurp "inputs/day10.inp"))) ; 1494

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

  (part-2 (str/split-lines test-inp-boojum)) ; 2044
  )

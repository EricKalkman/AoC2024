(ns aoc2024.day11)

(defn transition [stone]
  (cond
    (zero? stone) [1]
    (even? (count (str stone)))
    (let [s (str stone)
          len (count s)
          pivot (quot len 2)]
      (map parse-long [(subs s 0 pivot) (subs s pivot len)]))
    :else [(* 2024 stone)]))

(defn blinks-dumb [stones]
  (iterate #(mapcat transition %) stones))

(defn part-1-dumb [s]
  (let [bs (->> s
                (re-seq #"\d+")
                (map parse-long)
                blinks-dumb)]
    (-> bs (nth 25) count)))

(defn blink [stones]
  (->> stones
       (mapcat (fn [[label num]] (map #(vector % num) (transition label))))
       (reduce #(update %1 (first %2) (fn [x] (+' (or x 0) (second %2))))
               {})))

(defn part [n s]
  (let [bs (->> s
                (re-seq #"\d+")
                (map parse-long)
                (reduce #(update %1 %2 (fn [x] (inc (or x 0)))) {})
                (iterate blink))]
    (as-> bs $
      (nth $ n)
      (vals $)
      (reduce + $))))

(def part-1 (partial part 25))
(def part-2 (partial part 75))

(comment

  (transition 0) ; [1]
  (transition 1) ; [2024]
  (transition 1000) ; (10 0)

  (part-1-dumb "125 17") ; 55312

  (def start (->> [125 17] (map #(vector % 1)) (into {})))
  (blink start)

  (part-1 "125 17") ; 55312
  (part-1 (slurp "inputs/day11.inp")) ; 185205

  (part-2 (slurp "inputs/day11.inp")) ; 221280540398419
  
  )

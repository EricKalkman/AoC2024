(ns aoc2024.day03)

(def MULEXP #"mul\((\d+),(\d+)\)") 

(defn extract-muls [s]
  (->> (re-seq MULEXP s)
       (map (comp #(map parse-long %) rest))))

(defn part-1 [s]
  (->> s
       extract-muls
       (map #(reduce * 1 %))
       (reduce +)))

(defn part-2 [s]
  (loop [sq (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" s)
         state :do
         sum 0]
    (if-let [[op arg1 arg2] (first sq)]
      (cond
        (= "do()" op) (recur (rest sq) :do sum)
        (= "don't()" op) (recur (rest sq) :dont sum)
        (= :do state) (recur (rest sq)
                                state
                                (+ sum (* (parse-long arg1) (parse-long arg2))))
        :else (recur (rest sq) state sum))
      sum)))

(comment
  (part-1 (slurp "inputs/day03.inp")) ; 182619815

  (part-2 (slurp "inputs/day03.inp")) ; 80747545

  )

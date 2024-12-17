(ns aoc2024.day05
  (:require [aoc2024.parsecomb :as p]))

(def parse-edge (p/p-seq p/p-int (p/skip (p/chr \|)) p/p-int))
(def parse-rules (->> (p/list-+ parse-edge p/nl)
                      (p/p-map #(reduce (fn [m [from to]]
                                          (-> m
                                              (update from (fn [x] (conj (or x #{}) to)))
                                              (update to (fn [x] (or x #{})))))
                                        {} %))))
(def parse-pages (p/list-+ p/p-int (p/chr \,)))
(def parse-updates (p/list-+ parse-pages p/nl))
(def parse-input (->> (p/p-seq parse-rules p/skip-nl p/skip-nl parse-updates)
                      (p/label [:rules :updates])))

(defn right-order? [rules [from to]]
  ((rules from) to))

(defn part-1 [s]
  (let [{:keys [rules updates]} (->> s p/string->stringbuf parse-input :result)]
    (->> updates
         (filter #(every? (partial right-order? rules)
                          (partition 2 1 %)))
         (map #(nth % (quot (count %) 2)))
         (reduce +))))

(defn part-2 [s]
  (let [{:keys [rules updates]} (->> s p/string->stringbuf parse-input :result)]
    (->> updates
         (filter #(not (every? (partial right-order? rules)
                               (partition 2 1 %))))
         (map (comp #(nth % (quot (count %) 2))
                    #(sort (fn [a b] (boolean (right-order? rules [a b]))) %)))
         (reduce +))))

(comment
  (part-1 (slurp "inputs/day05.inp")) ; 4578

  (part-2 (slurp "inputs/day05.inp")) ; 6179
  )

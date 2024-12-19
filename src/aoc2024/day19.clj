(ns aoc2024.day19
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (let [[pats designs] (str/split (str/trim s) #"\n\n")]
    {:patterns (str/split pats #", ")
     :designs (str/split-lines designs)}))

(defn count-combinations [pats design]
  (loop [counts {-1 1} ; number of ways to make a substring of design ending at a position
         idx 0]
    (if (>= idx (count design))
      (counts (dec (count design)))
      (let [sub (subs design 0 (inc idx))
            cur-count (->> pats
                           (transduce
                             (comp (filter #(str/ends-with? sub %))
                                   (map #(counts (- idx (count %)) 0)))
                             + 0))]
        (recur (assoc counts idx cur-count)
               (inc idx))))))

(def count-combinations-recursive
  (memoize
    (fn [pats design]
      (if (empty? design)
        1
        (->> pats
             (filter #(str/starts-with? design %))
             (map #(count-combinations-recursive pats (subs design (count %))))
             (reduce +))))))

(defn part-1 [s]
  (let [{:keys [patterns designs]} (parse-input s)]
    (->> designs
         (map #(count-combinations patterns %))
         (filter (complement zero?))
         count)))

(defn part-2 [s]
  (let [{:keys [patterns designs]} (parse-input s)]
    (->> designs
         (map #(count-combinations patterns %))
         (reduce +))))

(comment
  (def test-inp (slurp "inputs/day19.test"))

  (let [{:keys [patterns designs]} (parse-input test-inp)]
    (map #(count-combinations-recursive patterns %) designs)) ; (2 1 4 6 0 1 2 0)
  (part-1 test-inp) ; 6
  (part-2 test-inp) ; 16
  (part-1 (slurp "inputs/day19.inp")) ; 236
  (time (part-2 (slurp "inputs/day19.inp"))) ; 643685981770598
  )

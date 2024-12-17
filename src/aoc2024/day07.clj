(ns aoc2024.day07
  (:require [clojure.string :as str]
            [aoc2024.parsecomb :as p]))

(def parse-numbers (->> (p/p-seq p/p-int (p/skip-string ": ")
                                   (p/list-+ p/p-int (p/chr \space) :vectorize? true))
                          (p/label [:target :terms])))
(def parse-input (p/list-+ parse-numbers p/nl))

(defn deconcat
  "Given a target number and a number to apply, calculates the inverse of the concatenation
  operation. That is, if x is a base-10 suffix of target, removes that suffix. If x is not
  a suffix of target, returns nil"
  [target x]
  (let [st (str target)
        sx (str x)]
    (if (str/ends-with? st sx)
      (parse-long (subs st 0 (- (count st) (count sx))))
      nil)))

;(defn inverse
;  "Returns a function undoes the application of x to target with op, where op is :+, :*, or :||"
;  [op x]
;  (case op
;    :+ (fn [y] (- y x))
;    :* (fn [y] (/ y x))
;    :|| (fn [y] (deconcat y x))))

(defn inverse
  "Undoes the application of x to target with op, where op is :+, :*, or :||"
  [op target x]
  (case op
    :+ (- target x)
    :* (let [d (quot target x)
             r (mod target x)]
         (if (zero? r) d nil))
    :|| (deconcat target x)))

(defn determine-ops
  "Determines which operations (choices given in ops) can be used to make the list of numbers
  in terms equal target. Returns a vector of [op num] pairs indicating opeerations. If no valid
  combination of ops exists, returns nil"
  [target terms ops]
  (if (= 1 (count terms)) ; if there is only one term left
    (if (= target (first terms)) ; if that term and the target are equal
      [[:1 target]]  ; :1 is meant to signify "identity", meaning no op applied
      nil)
    (->> ops
         (some #(let [next (peek terms)]
                  (some-> (inverse % target next)
                          (determine-ops (pop terms) ops)
                          (conj [% next])))))))

(defn part [ops s]
  (->> s
       p/string->stringbuf
       parse-input
       :result
       ; debug code to allow examination of operations
       ;(keep (fn [{:keys [target terms]}]
       ;        (some->> (solve target terms ops)
       ;                 (vector target))))))
       (filter (fn [{:keys [target terms]}] (determine-ops target terms ops)))
       (map :target)
       (reduce +)))

(def part-1 (partial part [:+ :*]))
(def part-2 (partial part [:+ :* :||]))

(comment
  (part-1 (slurp "inputs/day07.inp")) ; 66343330034722

  (part-2 (slurp "inputs/day07.inp")) ; 637696070419031
)

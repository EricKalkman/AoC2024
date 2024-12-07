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

(defn inverse
  "Returns a function undoes the application of x to target with op, where op is :+, :*, or :||"
  [op x]
  (case op
    :+ (fn [y] (- y x))
    :* (fn [y] (/ y x))
    :|| (fn [y] (deconcat y x))))

(defn determine-ops
  "Determines which operations (choices given in ops) can be used to make the list of numbers
  in terms equal target. Returns a vector of [op num] pairs indicating opeerations. If no valid
  combination of ops exists, returns nil"
  [target terms ops]
  (if (== 1 (count terms)) ; if there is only one term left
    (if (== target (first terms)) ; if that term and the target are equal
      [[:1 target]]  ; :1 is meant to signify "identity", meaning no op applied
      nil)
    (->> ops
         (some #(let [next (peek terms)
                      inv (inverse % next)]
                  (some-> (inv target)
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
  (def test-inp "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")
  
  (def inp (:result (parse-input (p/string->stringbuf test-inp))))

  (part-1 test-inp) ; 3749
; ([190 [[:1 10] [:* 19]]]
;  [3267 [[:1 81] [:* 40] [:+ 27]]]
;  [292 [[:1 11] [:+ 6] [:* 16] [:+ 20]]])
  (part-1 (slurp "inputs/day07.inp")) ; 66343330034722

  (part-2 test-inp) ; 11387
  (part-2 (slurp "inputs/day07.inp")) ; 637696070419031
)

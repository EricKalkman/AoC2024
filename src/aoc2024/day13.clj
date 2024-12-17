(ns aoc2024.day13
  (:require [aoc2024.parsecomb :as p]
            [aoc2024.searches :as search]
            [aoc2024.grids :as g]
            [clojure.math.numeric-tower :as m]
            [clojure.string :as str]))

(defn button-parser [id]
  (p/p-seq (p/skip-string "Button ") (p/skip-string id) (p/skip-string ": X+")
           p/p-int (p/skip-string ", Y+") p/p-int))

(def claw-machine-parser (->> (p/p-seq (button-parser "A") p/skip-nl (button-parser "B") p/skip-nl
                                       (p/p-seq (p/skip-string "Prize: X=") p/p-int (p/skip-string ", Y=") p/p-int))
                              (p/label [:a :b :prize])))

(def parse-input (p/list-+ claw-machine-parser (p/p-repeat 2 p/nl)))

(def A-COST 3)
(def B-COST 1)

(defn neighbors [a b prize coord]
  (->> [[(g/coord+ a coord) A-COST] [(g/coord+ b coord) B-COST]]
       (filter (fn [[[row col] _]]
                 (and (<= row (first prize))
                      (<= col (second prize)))))))

(defn part-1-sorta-dumb [s]
  (->> s
       p/string->stringbuf
       parse-input
       :result
       (pmap (fn [{:keys [a b prize]}]
               (-> (search/dijkstra (partial neighbors a b prize) [[0 0]]
                                    #(= % prize))
                   :prevs
                   (get prize)
                   first
                   first)))
       (filter identity)
       (reduce +)))

; right, now to actually sit down and be smart about the problem, lol
; | p_x |  =  | a_x  b_x | | m |
; | p_y |  =  | a_y  b_y | | n |
; Turns out, there is at most one solution to the above equation if a and b are linearly
; independent (i.e., not colinear) because linear algebra

(defn make-inverse-mat [[ax ay] [bx by]]
  (let [det (- (* ax by) (* bx ay))]
    (if (zero? det)
      nil
      ; row-major
      [[(/ by det) (/ (- bx) det)]
       [(/ (- ay) det) (/ ax det)]])))

(defn calc-na-nb [[[a b] [c d]] [prize-x prize-y]]
  [(+ (* a prize-x) (* b prize-y)) (+ (* c prize-x) (* d prize-y))])

(defn solve-colinear [p a b]
  ; brute-forces the minimum token cost for all m, n that satisfy
  ; m*a + n*b = p by iterating over all possible m and calculating n
  (if (not (zero? (rem p (m/gcd a b))))
    nil
    (loop [ma (- p (mod p a))
           min-cost Long/MAX_VALUE]
      (if (<= ma 0)
        min-cost
        (let [remaining (- p ma)]
          (if (zero? (mod remaining b))
            (recur (- ma a) (min min-cost (+ (* A-COST (/ ma a)) (* B-COST (/ remaining b)))))
            (recur (- ma a) min-cost)))))))

(defn num-coins-needed [[ax ay :as a] [bx _by :as b] [px py :as prize]]
  (let [mat (make-inverse-mat a b)]
    (cond
      (some? mat)  ; if a and b aren't co-linear
      (let [[n-a n-b] (calc-na-nb mat prize)]
        (or (and (integer? n-a) (integer? n-b) (+ (* n-a A-COST) (* n-b B-COST)))
            nil))
      ; mat = nil -> a and b are co-linear. is the prize on the line defined by a and b?
      (= (/ px ax) (/ py ay)) (solve-colinear px ax bx)
      ; if not, there is no solution
      :else nil)))

(defn reposition-prize [n prize]
  (g/coord+ prize [n n]))


(defn part [shift s]
  (->> s
       p/string->stringbuf
       parse-input
       :result
       (keep (fn [{:keys [a b prize]}] (num-coins-needed a b (reposition-prize shift prize))))
       (reduce +)))

(def part-1 (partial part 0))
(def part-2 (partial part 10000000000000))

(comment
  (part-1-sorta-dumb (str/trim (slurp "inputs/day13.inp"))) ; 28262

  (part-1 (str/trim (slurp "inputs/day13.inp"))) ; 28262N

  (part-2 (str/trim (slurp "inputs/day13.inp"))) ; 101406661266314N

  (part-1 "Button A: X+20, Y+52
Button B: X+5, Y+13
Prize: X=110, Y=286") ; 17
  )

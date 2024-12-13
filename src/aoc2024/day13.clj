(ns aoc2024.day13
  (:require [aoc2024.parsecomb :as p]
            [aoc2024.searches :as search]
            [aoc2024.grids :as g]
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

(defn num-coins-needed [[ax ay :as a] [bx by :as b] [px py :as prize]]
  (let [mat (make-inverse-mat a b)]
    (cond
      (some? mat)  ; if a and b aren't co-linear
      (let [[n-a n-b] (calc-na-nb mat prize)]
        (or (and (integer? n-a) (integer? n-b) (+ (* n-a A-COST) (* n-b B-COST)))
            nil))
      ; if a and b are co-linear, check first if b alone can be used to reach the prize
      (and (zero? (mod px bx)) (zero? (mod py by)) (= (/ px bx) (/ py by))) (* B-COST (/ px bx))
      ; else check if a lone can be used to reach the prize
      (and (zero? (mod px ax)) (zero? (mod py ay)) (= (/ px ax) (/ py ay))) (* A-COST (/ px ax))
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
  (def test-inp "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

  (part-1-sorta-dumb test-inp) ; 480
  (part-1-sorta-dumb (str/trim (slurp "inputs/day13.inp"))) ; 28262

  (part-1 test-inp) ; 480N
  (part-1 (str/trim (slurp "inputs/day13.inp"))) ; 28262N

  (part-2 test-inp) ; 875318608908N
  (part-2 (str/trim (slurp "inputs/day13.inp"))) ; 101406661266314N
  )

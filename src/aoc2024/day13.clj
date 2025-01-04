(ns aoc2024.day13
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [aoc2024.parsecomb :as p]
            [aoc2024.searches :as search]
            [aoc2024.grids :as g]
            [clojure.math.numeric-tower :as m]
            [clojure.string :as str]
            [structural.core :as s])
  (:import [aoc2024.grids vec2]))

(defn button-parser [id]
  (->> (p/p-seq (p/skip-string "Button ") (p/skip-string id) (p/skip-string ": X+")
            p/p-int (p/skip-string ", Y+") p/p-int)
       (p/p-map (partial apply g/->vec2))))

(def claw-machine-parser (->> (p/p-seq (button-parser "A") p/skip-nl (button-parser "B") p/skip-nl
                                       (->> (p/p-seq (p/skip-string "Prize: X=") p/p-int
                                                     (p/skip-string ", Y=") p/p-int)
                                            (p/p-map (partial apply g/->vec2))))
                              (p/label [:a :b :prize])))

(def parse-input (p/list-+ claw-machine-parser (p/p-repeat 2 p/nl)))

(def A-COST 3)
(def B-COST 1)

(defn neighbors [^vec2 a ^vec2 b ^vec2 prize ^vec2 coord]
  (->> [[(g/vec2+ a coord) A-COST] [(g/vec2+ b coord) B-COST]]
       (filter (fn [[^vec2 p _]]
                 (s/with-slots [{:fields [row col]} ^vec2 p]
                   (and (<= row (.row prize))
                        (<= col (.col prize))))))))

(defn part-1-sorta-dumb [s]
  (->> s
       p/string->stringbuf
       parse-input
       :result
       (pmap (fn [{:keys [a b prize]}]
               (-> (search/dijkstra (partial neighbors a b prize) [(g/->vec2 0 0)]
                                    #(= ^vec2 % prize))
                   :prevs
                   (get prize)
                   :cost)))
       (filter identity)
       (reduce +)
       ))

; right, now to actually sit down and be smart about the problem, lol
; | p_x |  =  | a_x  b_x | | m |
; | p_y |  =  | a_y  b_y | | n |
; Turns out, there is at most one solution to the above equation if a and b are linearly
; independent (i.e., not colinear) because linear algebra

(defn make-inverse-mat [^vec2 a ^vec2 b]
  (s/with-slots [{:fields [row col]} ^vec2 a
                 ax row ay col
                 {:fields [row col]} ^vec2 b
                 bx row by col]
    (let [det (- (* ax by) (* bx ay))]
      (if (zero? det)
        nil
        ; row-major
        [[(/ by det) (/ (- bx) det)]
         [(/ (- ay) det) (/ ax det)]]))))

(defn calc-na-nb [[[a b] [c d]] [^long prize-x ^long prize-y]]
  [(+ (* a prize-x) (* b prize-y)) (+ (* c prize-x) (* d prize-y))])

(defn solve-colinear [^long p ^long a ^long b]
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

(defn num-coins-needed [[^long ax ^long ay :as a] [^long bx _by :as b] [^long px ^long py :as prize]]
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

(defn reposition-prize [^long n ^vec2 prize] ^vec2
  (g/vec2+ prize (g/->vec2 n n)))

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
  (time (part-1-sorta-dumb (str/trim (slurp "inputs/day13.test")))) ; 480

  (time (part-1 (str/trim (slurp "inputs/day13.inp")))) ; 28262N

  (time (part-2 (str/trim (slurp "inputs/day13.inp")))) ; 101406661266314N

  (part-1 "Button A: X+20, Y+52
Button B: X+5, Y+13
Prize: X=110, Y=286") ; 17
  )

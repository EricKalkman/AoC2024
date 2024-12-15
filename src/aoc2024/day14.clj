(ns aoc2024.day14
  (:require [aoc2024.parsecomb :as p]
            [clojure.string :as str]
            [clojure.math :as m]))

(def parse-coord (p/p-seq p/p-int (p/skip-string ",") p/p-int))
(def parse-robot (->> (p/p-seq (p/skip-string "p=") parse-coord
                               (p/skip-string " v=") parse-coord)
                      (p/label [:pos :vel])))
(def parse-input (p/list-+ parse-robot p/nl))

; note: internally uses row, col; problem uses x, y, which is backwards
(def HEIGHT 103)
(def WIDTH 101)

(defn step-robot [w h {[vr vc] :vel :as robot}]
  (update robot :pos (fn [[row col]] [(mod (+ row vr) w) (mod (+ col vc) h)])))
(defn step-system [w h robots] (map (partial step-robot h w) robots))
; note: had to increase java stack size. I guess iterate does that
(defn steps [w h robots] (iterate (partial step-system h w) robots))

(defn quadrant [w h {[x y] :pos}]
  (let [half-h (quot h 2)
        half-w (quot w 2)]
    (if (or (== half-w x) (== half-h y))
      nil
      (+ (* 2 (quot y (inc half-h)))
         (quot x (inc half-w))))))

;(defn part-1 [w h s]
(defn part-1 [s]
  (as-> s $
    (p/string->stringbuf $)
    (parse-input $)
    (:result $)
    (steps WIDTH HEIGHT $)
    (nth $ 100)
    (group-by (partial quadrant WIDTH HEIGHT) $)
    (dissoc $ nil) ; remove robots on middle lines
    (vals $)
    (map count $)
    (reduce * $)))

(defn entropies [robots]
  (let [n (count robots)
        xs (->> (map (comp first :pos) robots) (group-by identity) vals (map count))
        ys (->> (map (comp second :pos) robots) (group-by identity) vals (map count))
        ex (->> xs
                (map #(* (/ % n) (m/log (/ % n))))
                (reduce - 0))
        ey (->> ys
                (map #(* (/ % n) (m/log (/ % n))))
                (reduce - 0))]
    [ex ey]
    ))

(defn plot-robots [w h n robots]
  (println n)
  (let [rs (into #{} (map :pos robots))]
    (doseq [y (range h)]
      (doseq [x (range w)]
        (if (rs [x y])
          (print \#)
          (print \.)))
      (println ""))))

(defn mult-inverse [x n]
  ; wiki. obviously.
  (loop [t 0
         new_t 1
         r n
         new_r x]
    (if (zero? new_r)
      (cond
        (> r 1) nil
        (< t 0) (+ t n)
        :else t)
      (let [q (quot r new_r)]
        (recur new_t (- t (* q new_t))
               new_r (- r (* q new_r)))))))

; n = x (mod w) 
; n = x + k w
; n = y (mod h)
; x + k w = y (mod h)
; k w = y - x (mod h)
; k = (w^-1) (y - x) (mod h)

;(defn part-2 [w h s]
(defn part-2 [s]
  (let [ss (->> s
                p/string->stringbuf
                parse-input
                :result
                (steps WIDTH HEIGHT))
        ents (->> ss
                  (take (max WIDTH HEIGHT))
                  (map-indexed #(vector %1 (entropies %2))))
        min-x (->> (apply min-key (comp first second) (reverse ents)) first)
        min-y (->> (apply min-key (comp second second) (reverse ents)) first)
        k (mod (* (mult-inverse WIDTH HEIGHT) (- min-y min-x)) HEIGHT)
        n (+ min-x (* k WIDTH))]
    ;(plot-robots WIDTH HEIGHT 77 (nth ss 77))
    n))

(comment
  (def test-inp "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")
  ; safety factor: 12
  (def robots (->> test-inp p/string->stringbuf parse-input :result))
  (first robots) ; {:pos [0 4], :vel [3 -3]}

  ;(part-1 test-inp) ; 12
  (part-1 (str/trim (slurp "inputs/day14.inp"))) ; 226548000

  (part-2 (str/trim (slurp "inputs/day14.inp"))) ; 7753

  )

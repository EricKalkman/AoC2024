(ns aoc2024.day21
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [aoc2024.grids :as g]
            [structural.core :as s]
            [clojure.string :as str])
  (:import [aoc2024.grids vec2]))

(def numpad {\7 (g/->vec2 0 0) \8 (g/->vec2 0 1) \9 (g/->vec2 0 2)
             \4 (g/->vec2 1 0) \5 (g/->vec2 1 1) \6 (g/->vec2 1 2)
             \1 (g/->vec2 2 0) \2 (g/->vec2 2 1) \3 (g/->vec2 2 2)
                               \0 (g/->vec2 3 1) \A (g/->vec2 3 2)})

(def dirpad {:up (g/->vec2 0 1) \A (g/->vec2 0 2) :left (g/->vec2 1 0)
             :down (g/->vec2 1 1) :right (g/->vec2 1 2)})

(def NUMPAD-SPACE (g/->vec2 3 0))
(def DIRPAD-SPACE (g/->vec2 0 0))

;;; Rewritten for dynamic programming because a construtive solution is not
;;; gonna cut it lol

(defn build-initial-button-presses [height]
  (-> (for [from (keys dirpad)
            to-press (keys dirpad)]
        [from to-press (inc height)])
      (zipmap (repeat 1))))

(defn move-arm-incremental
  "returns both full button sequences for a `min` call later because I'm so done
  trying to be smart about it"
  [coord-map ^vec2 space-coord from to]
  (s/with-slots [{:fields [row col] :as a} ^vec2 (coord-map from)
                 ar row ac col
                 {:fields [row col] :as b} ^vec2 (coord-map to)
                 br row bc col
                 {:fields [row col]} ^vec2 (g/vec2- b a)
                 dr row dc col
                 dir1 (if (< dr 0) :up :down)
                 dir2 (if (< dc 0) :left :right)]
    (->>
      [[(g/->vec2 br ac) (concat (repeat (abs dr) dir1) (repeat (abs dc) dir2))]
       [(g/->vec2 ar bc) (concat (repeat (abs dc) dir2) (repeat (abs dr) dir1))]]
      (filter #(not= space-coord ^vec2 (first %)))
      (mapv second))))

(defn update-presses-for-height [coord-map ^vec2 space-coord ^long height n-moves-to-press]
  ; for every combination of buttons (order matters)
  (->> (for [from (keys coord-map)
             to-press (keys coord-map)]
         ; get the up to two different sequences of button presses necessary to
         ; effect movement at the current depth
         (->> (move-arm-incremental coord-map space-coord from to-press)
              ; a movement will always start on A and end on pressing A
              (map #(->> (cons \A (concat % [\A]))
                         (partition 2 1)   ; goated function
                         ; consult previous iteration to see how many presses are needed to effect
                         ; its own entry
                         (map (fn [[from* to*]] (n-moves-to-press [from* to* (inc height)])))
                         (reduce +)))
              ; pick the minimum number of movements
              (apply min)
              (vector [from to-press height])))
       ; update the DP state
       (into n-moves-to-press)))

(defn dirpad-moves-dp [^long height s]
  (loop [height height
         ; DP state; [from-button, button-to-press, height] -> number of presses required to
         ; press button-to-press
         n-moves-to-press (build-initial-button-presses height)]
    (cond
      (== height -1)
      (->> (cons \A s) ; start on A
           (partition 2 1) ; pair up
           ; get moves required for each numerical press
           (map (fn [[from to]] (n-moves-to-press [from to 0])))
           (reduce +))

      (zero? height)
      (recur (dec height)
             (update-presses-for-height numpad NUMPAD-SPACE height n-moves-to-press))
      :else
      (recur (dec height)
             (update-presses-for-height dirpad DIRPAD-SPACE height n-moves-to-press)))))

(defn part-dp [^long n lines]
  (->> lines
       (map (partial dirpad-moves-dp n))
       (map #(* (parse-long (subs %1 0 (dec (count %1)))) %2) lines)
       (reduce +)))

(def part-1 (partial part-dp 2))
(def part-2 (partial part-dp 25))

(comment
  (def test-inp "029A")
  (def test-inp-full (slurp "inputs/day21.test"))

  (def test-lines (str/split-lines test-inp-full))

  (count "<A^A^>^AvvvA") ; 12
  (count "v<<A>>^A<A>AvA<^AA>A<vAAA>^A") ; 28
  (count "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A") ; 68
  (count "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A") ; 64

  (part-1 test-lines) ; 126384
  (time (part-1 (str/split-lines (slurp "inputs/day21.inp")))) ; 219366

  (part-dp 2 test-lines) ; 126384
  (part-dp 2 (str/split-lines (slurp "inputs/day21.inp"))) ; 219366
  (time (part-dp 25 (str/split-lines (slurp "inputs/day21.inp")))) ; 271631192020464
)

(ns aoc2024.day21
  (:require [aoc2024.grids :as g]
            [clojure.string :as str]))

(def numpad {\7 [0 0] \8 [0 1] \9 [0 2]
             \4 [1 0] \5 [1 1] \6 [1 2]
             \1 [2 0] \2 [2 1] \3 [2 2]
                      \0 [3 1] \A [3 2]})

(def dirpad {:up [0 1] \A [0 2] :left [1 0] :down [1 1] :right [1 2]})

(def NUMPAD-SPACE [3 0])
(def DIRPAD-SPACE [0 0])

;;; NOTE: anything below this point to the next ;;; is not used in the final solution

(def prio {:left 0 :down 1 :right 2 :up 3})

(def move-arm
  (memoize
    (fn [from to]
      (let [[dr dc :as delta] (g/coord- to from)
            rowdir (g/dir-of [dr 0])
            coldir (g/dir-of [0 dc])]
        (->> (map vector [rowdir coldir] (map abs delta))
             (filter first)
             (into (sorted-map-by #(< (prio %1) (prio %2)))))))))

(defn unpanic [spaces cur move]
  (let [[[d1 n1] [d2 n2]] (seq move)]
   (if (and d2 (= (g/move d1 n1 cur) spaces))
     [[d2 n2] [d1 n1]]
     move)))

(def get-movements
  (identity
    (fn [cur b]
      (let [moves (->> (map first b)
                       (cons cur)
                       (map dirpad)
                       (partition 2 1)
                       (mapv (fn [[c1 c2]]
                               (unpanic DIRPAD-SPACE c1 (move-arm c1 c2)))))]
        [(last (map first b))
         (mapcat #(vector %1 {\A %2}) moves (map second b))]))))

(defn dirpad-moves [height s]
  (if (zero? height)
    (as-> (str \A s) $
      (map numpad $)
      (partition 2 1 $)
      (map (fn [[cur next]]
             (let [move (move-arm cur next)]
               (unpanic NUMPAD-SPACE cur move))) $)
      (interleave $ (repeat {\A 1})))
    (let [buttons (dirpad-moves (dec height) s)]
      (loop [buttons buttons
             cur \A
             result []]
        (if-let [b (first buttons)]
          (let [[next moves] (get-movements cur b)]
            (recur
              (rest buttons)
              next
              (into result moves)))
          result)))))

(defn part-constructive [n lines]
  (->> lines
       (map (comp
              (partial reduce +)
              (partial map second)
              (partial mapcat seq)
              (partial dirpad-moves n)))
       (map #(* (parse-long (subs %1 0 (dec (count %1)))) %2) lines)
       (reduce +)))

;(def part-1 (partial part-constructive 2))

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
  [coord-map space-coord from to]
  (let [[ar ac :as a] (coord-map from)
        [br bc :as b] (coord-map to)
        [dr dc] (g/coord- b a)
        dir1 (if (< dr 0) :up :down)
        dir2 (if (< dc 0) :left :right)]
    (->>
      [[[br ac] (concat (repeat (abs dr) dir1) (repeat (abs dc) dir2))]
       [[ar bc] (concat (repeat (abs dc) dir2) (repeat (abs dr) dir1))]]
      (filter #(not= (first %) space-coord))
      (mapv second))))

(defn update-presses-for-height [coord-map space-coord height n-moves-to-press]
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

(defn dirpad-moves-dp [height s]
  (loop [height height
         ; DP state; [from-button, button-to-press, height] -> number of presses required to
         ; press button-to-press
         n-moves-to-press (build-initial-button-presses height)]
    (cond
      (= height -1)
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

(defn part-dp [n lines]
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
  (part-1 (str/split-lines (slurp "inputs/day21.inp"))) ; 219366

  (part-dp 2 test-lines) ; 126384
  (part-dp 2 (str/split-lines (slurp "inputs/day21.inp"))) ; 219366
  (part-dp 25 (str/split-lines (slurp "inputs/day21.inp"))) ; 271631192020464
)

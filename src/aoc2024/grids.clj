(ns aoc2024.grids
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [structural.core :as s]
            [clojure.string :as str])
  (:import [clojure.lang Counted Indexed Associative IPersistentVector IFn MapEntry
            Seqable Reversible Keyword]))

(defrecord vec2 [^long row ^long col]
  Indexed
  (nth [_ i] (case i 0 row 1 col))
  (nth [_ i default] (case i 0 row 1 col default))
  
  Comparable
  (compareTo [_ other]
    (let [c (compare row (.row ^vec2 other))]
      (case c
        0 (compare col (.col ^vec2 other))
        c))))

(defn vec2+ [a b] ^vec2
  (s/with-slots [{:fields [row col]} ^vec2 a
                 x1 row y1 col
                 {:fields [row col]} ^vec2 b]
    (->vec2 (+ x1 row) (+ y1 col))))

(defn vec2- [a b] ^vec2
  (s/with-slots [{:fields [row col]} ^vec2 a
                 x1 row y1 col
                 {:fields [row col]} ^vec2 b]
    (->vec2 (- x1 row) (- y1 col))))

(def DIRS4 [:up :right :down :left])
(def DIRS8 [:up :right :down :left :NE :SE :SW :NW])
(defn dir->delta [^Keyword dir] ^vec2
  (case dir
    :up (->vec2 -1 0)
    :right (->vec2 0 1)
    :down (->vec2 1 0)
    :left (->vec2 0 -1)
    :NE (->vec2 -1 1)
    :SE (->vec2 1 1)
    :SW (->vec2 1 -1)
    :NW (->vec2 -1 -1)))

(defn neighbors4 [^vec2 p]
  (mapv #(vec2+ p (dir->delta %)) DIRS4))

(defn turn-right [^Keyword dir] ^Keyword
  (case dir
    :up :right
    :right :down
    :down :left
    :left :up
    :NE :SE
    :SE :SW
    :SW :NW
    :NW :NE))

(defn turn-left [^Keyword dir] ^Keyword
  (case dir
    :right :up
    :down :right
    :left :down
    :up :left
    :SE :NE
    :SW :SE
    :NW :SW
    :NE :NW))

(defn flip-dir [^Keyword dir] ^Keyword
  (case dir
    :up :down
    :right :left
    :down :up
    :left :right
    :NE :SW
    :SE :NW
    :SW :NE
    :NW :SE))

(defn step [dir ^long k] ^vec2
  (s/with-slots [{:fields [row col]} ^vec2 (dir->delta dir)]
    (->vec2 (* k row) (* k col))))

(defn move [dir ^long n ^vec2 p] ^vec2
  (vec2+ p (step dir n)))

(deftype Grid [^IPersistentVector arr ^long height ^long width]
  Counted
  (count [_] (.count arr))
  
  Associative
  (containsKey [_ k]
    (s/with-slots [{:fields [row col]} ^vec2 k]
      (and (>= row 0) (< row height) (>= col 0) (< col width))))
  (entryAt [_ k]
    (s/with-slots [{:fields [row col]} ^vec2 k
                   idx ^long (+ col (* width row))]
      (MapEntry. k (.nth arr idx))))
  (assoc [_ k v]
    (s/with-slots [{:fields [row col]} ^vec2 k
                   idx ^long (+ col (* width row))]
      (Grid. (.assoc arr idx v) height width)))
  (valAt [_ k]
    (s/with-slots [{:fields [row col]} ^vec2 k
                   idx ^long (+ col (* width row))]
      (.nth arr idx)))
  (valAt [this k default]
    (if (.containsKey ^Grid this k)
      (s/with-slots [{:fields [row col]} ^vec2 k
                     idx ^long (+ col (* width row))]
        (.nth arr idx))
      default))

  Seqable
  (seq [_]
    (for [row (range height)
          col (range width)]
      (let [c ^vec2 (->vec2 row col)
            idx (+ col (* row width))]
        (MapEntry. c (.nth arr idx)))))

  Reversible
  (rseq [_]
    (for [row (range (dec height) -1 -1)
          col (range (dec width) -1 -1)]
      (let [c ^vec2 (->vec2 row col)
            idx (+ col (* row width))]
        (MapEntry. c (.nth arr idx)))))

  IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k default] (.valAt this k default))
)

(defn lines->grid [lines] ^Grid
  (let [height (count lines)
        width (count (first lines))]
    (assert (every? #(= width (count %)) lines))
    (Grid. (into [] (apply concat lines)) height width)))

(defn height [^Grid g] ^long (.height g))
(defn width [^Grid g] ^long (.width g))

(defn fold-grid-keys [f init ^Grid g]
  (let [h ^long (height g)
        w ^long (width g)]
    (loop [acc init
           row 0
           col 0]
      (cond
        (>= row h) acc
        (>= col w) (recur acc (inc row) 0)
        :else (recur (f acc (->vec2 row col)) row (inc col))))))

(defn fold-grid-values [f init ^Grid g]
  (let [len ^long (* (height g) (width g))]
    (loop [acc init
           idx 0]
      (if (>= idx len)
        acc
        (recur (f acc (.nth ^IPersistentVector (.arr g) idx)) (inc idx))))))

(defn fold-grid-kv [f init ^Grid g]
  (let [h ^long (height g)
        w ^long (width g)]
    (loop [acc init
           row 0
           col 0]
      (cond
        (>= row h) acc
        (>= col w) (recur acc (inc row) 0)
        :else
        (let [p ^vec2 (->vec2 row col)
              idx (+ col (* w row))]
          (recur (f acc p (.nth ^IPersistentVector (.arr g) idx))
                 row (inc col)))))))

(defn first-coord-of [^Grid g p]
  (let [len ^long (* (height g) (width g))]
    (loop [idx 0]
      (cond
        (>= idx len) nil
        (p (.nth ^IPersistentVector (.arr g) idx))
        (->vec2 (quot idx (width g)) (mod idx (width g)))
        :else (recur (inc idx))))))

(defn all-coords-of [^Grid g p]
  (persistent! (fold-grid-kv #(if (p %3) (conj! %1 %2) %1) (transient []) g)))

(comment
  (def lines (str/split-lines (slurp "inputs/day06.test")))
  (def real-lines (str/split-lines (slurp "inputs/day06.inp")))

  (let [g ^Grid (lines->grid lines) ]
    (->> (seq g)
         (filter #(= \# (second %))))
    )

  )

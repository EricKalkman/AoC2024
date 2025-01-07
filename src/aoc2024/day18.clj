(ns aoc2024.day18
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [aoc2024.searches :as search]
            [structural.core :as s]
            [clojure.string :as str]
            [aoc2024.grids :as g])
  (:import [aoc2024.grids vec2]
           [clojure.lang PersistentVector]))

(defn parse-input [s] (->> s (re-seq #"\d+") (map parse-long) (partition 2)
                           (map (partial apply g/->vec2))))

(defn trace-path [prevs ^vec2 dst ^vec2 src]
  (and (prevs dst)
       (->> (iterate prevs dst)
            (take-while #(not= src ^vec2 %)))))

(defn find-path [coords w h ^vec2 src ^vec2 dst]
  (-> (search/bfs #(->> (g/neighbors4 %)
                        (filterv
                          (fn [n]
                            (s/with-slots [{:fields [row col]} ^vec2 n
                                           x row y col]
                              (and (>= x 0) (< x w)
                                   (>= y 0) (< y h)
                                   (not (coords n)))))))
                  src #(= dst ^vec2 %))
      :prevs
      (trace-path dst src)))

(defn part-1-with-pars [^long w ^long h ^long n ^String s]
  (let [coords (into #{} (take n) (parse-input (str/trim s)))
        src (g/->vec2 0 0)
        dst (g/->vec2 (dec w) (dec h))]
    (-> (find-path coords w h src dst)
        count)))

(def part-1 (partial part-1-with-pars 71 71 1024))

(defn part-2-with-pars [^long w ^long h ^long n ^String s]
  (let [coords (parse-input (str/trim s))
        coord-set (into #{} (take n) coords)
        src (g/->vec2 0 0)
        dst (g/->vec2 (dec w) (dec h))]
    (loop [coords-to-drop (drop n coords)
           n-dropped n
           coord-set (transient coord-set)
           current-path (some->> (find-path coord-set w h src dst) (into #{}))
           n-bfs 1]
      (if current-path
        (if-let [to-drop (first coords-to-drop)]
          (if (current-path to-drop)
            (recur (rest coords-to-drop)
                   (inc n-dropped)
                   (conj! coord-set to-drop)
                   (some->> (find-path (conj! coord-set to-drop) w h src dst) (into #{}))
                   (inc n-bfs))
            (recur (rest coords-to-drop) (inc n-dropped) (conj! coord-set to-drop) current-path
                   n-bfs))
          (assert false "exhausted all falling blocks"))
        (->> (.nth ^PersistentVector coords (dec n-dropped))
             vals
             (str/join ","))))))

(defn binary-search [^Long w ^Long h ^vec2 src ^vec2 dst coords ^Long lo ^Long hi]
  (if (<= hi lo)
    lo
    (let [n (quot (+ lo hi) 2)
          cs (into #{} (subvec coords 0 n))]
      (if (find-path cs w h src dst)
        (recur w h src dst coords (inc n) hi)
        (recur w h src dst coords lo n)))))

(def part-2 (partial part-2-with-pars 71 71 1024))

(defn part-2-bs-with-pars [^long w ^long h s]
  (let [coords (vec (parse-input (str/trim s)))
        src (g/->vec2 0 0)
        dst (g/->vec2 (dec w) (dec h))]
    (->> (binary-search w h src dst coords 0 (count coords))
         dec
         (.nth ^PersistentVector coords)
         vals
         (str/join ","))))

(def part-2-bs (partial part-2-bs-with-pars 71 71))

(comment
  (part-1-with-pars 7 7 12 (slurp "inputs/day18.test")) ; 22
  (part-2-with-pars 7 7 12 (slurp "inputs/day18.test")) ; "6,1"

  (time (part-1-with-pars 71 71 1024 (slurp "inputs/day18.inp"))) ; 446
  (time (part-1 (slurp "inputs/day18.inp"))) ; 446
  (time (part-2-with-pars 71 71 1024 (slurp "inputs/day18.inp"))) ; "39,40"
  (time (part-2 (slurp "inputs/day18.inp"))) ; "39,40"
; ":n-bfs,65,:n-dropped,2878,39,40"
  (time (part-2-bs-with-pars 71 71 (slurp "inputs/day18.inp"))) ; "39,40" (depth 12)
  (time (part-2-bs (slurp "inputs/day18.inp"))) ; "39,40"
  )

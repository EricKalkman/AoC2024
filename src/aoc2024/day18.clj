(ns aoc2024.day18
  (:require [aoc2024.searches :as search]
            [clojure.string :as str]
            [aoc2024.grids :as g]))

(defn parse-input [s] (->> s (re-seq #"\d+") (map parse-long) (partition 2) (map vec)))

(defn trace-path [prevs dst src]
  (and (prevs dst)
       (->> (iterate prevs dst)
            (take-while #(not= % src)))))

(defn find-path [coords w h src dst]
  (-> (search/bfs #(->> (g/neighbors4 %)
                        (filterv
                          (fn [[x y :as n]]
                            (and (<= 0 x (dec w))
                                 (<= 0 y (dec h))
                                 (not (coords n))))))
                  src #(= % dst))
      :prevs
      (trace-path dst src)))

(defn part-1-with-pars [w h n s]
  (let [coords (into (sorted-set) (take n) (parse-input (str/trim s)))
        src [0 0]
        dst [(dec w) (dec h)]]
    (-> (find-path coords w h src dst)
        count)))

(def part-1 (partial part-1-with-pars 71 71 1024))

(defn part-2-with-pars [w h n s]
  (let [coords (parse-input (str/trim s))
        coord-set (into (sorted-set) (take n) coords)
        src [0 0]
        dst [(dec w) (dec h)]]
    (loop [coords-to-drop (drop n coords)
           n-dropped n
           coord-set coord-set
           current-path (some->> (find-path coord-set w h src dst) (into (sorted-set)))]
      (if current-path
        (if-let [to-drop (first coords-to-drop)]
          (if (current-path to-drop)
            (recur (rest coords-to-drop)
                   (inc n-dropped)
                   (conj coord-set to-drop)
                   (some->> (find-path (conj coord-set to-drop) w h src dst) (into (sorted-set))))
            (recur (rest coords-to-drop) (inc n-dropped) (conj coord-set to-drop) current-path))
          (assert false "exhausted all falling blocks"))
        (->> (nth coords (dec n-dropped))
             (str/join ","))))))

(def part-2 (partial part-2-with-pars 71 71 1024))

(comment
  (part-1-with-pars 7 7 12 (slurp "inputs/day18.test")) ; 22
  (part-2-with-pars 7 7 12 (slurp "inputs/day18.test")) ; "6,1"

  (part-1-with-pars 71 71 1024 (slurp "inputs/day18.inp")) ; 446
  (part-1 (slurp "inputs/day18.inp")) ; 446
  (part-2-with-pars 71 71 1024 (slurp "inputs/day18.inp")) ; "39,40"
  (part-2 (slurp "inputs/day18.inp")) ; "39,40"
  )

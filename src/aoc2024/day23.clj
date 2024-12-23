(ns aoc2024.day23
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-graph [lines]
  (->> lines
       (mapcat #(let [[a b] (str/split % #"-")]
                  [[a b] [b a]]))
       (reduce #(update %1 (first %2)
                        (fn [set] (conj (or set #{}) (second %2))))
               {})))

(defn dissoc-vert [g v]
  (as-> (g v) $
        (reduce #(update %1 %2 (fn [set] (disj set v))) g $)
        (dissoc $ v)))

(defn clique-3 [g]
  ; note: sorting useless for test input
  (loop [g g
         vs (sort-by (comp - count g) (keys g))
         triangles []]
    (if-let [v (first vs)]
      (let [neighs (g v)
            neighs2
            (into #{}
                  (mapcat #(->> (set/intersection neighs (g %))
                                (map (fn [n2] #{ % n2 }))))
                  neighs)]
        (recur (dissoc-vert g v)
               (rest vs)
               (into triangles
                     (map #(conj % v) neighs2))))
      triangles)))

(defn part-1 [lines]
  (->> lines
       parse-graph
       clique-3
       (filter #(some (fn [name] (str/starts-with? name "t")) %))
       count))

;; *sigh* I kinda figured were were heading for the maximum clique

(defn bron-korbosch-helper
  "Helper function to return all maximal cliques of g. Initialize with
  (bron-korbosch-helper g #{} (set (keys g)) #{})."
  [g clique-candidate possible-extensions excluded]
  (if (and (empty? possible-extensions) (empty? excluded))
    [clique-candidate]
    (loop [cliques []
           p possible-extensions]
      (if-let [v (first p)]
        (recur (into cliques
                     ; find all maximal cliques containing v
                     (bron-korbosch-helper
                       g
                       ; add v to current candidate clique
                       (conj clique-candidate v)
                       ; narrow possible extensions to only neighbors of v
                       (set/intersection p (g v))
                       ; narrow possible exclusions only to neighbors of v
                       (set/intersection excluded (g v))))
               (disj p v))
        cliques))))

(defn bron-korbosch
  "Returns a vector of all maximal (not maximum) cliques"
  [g]
  (bron-korbosch-helper g #{} (set (keys g)) #{}))

(defn part-2 [lines]
  (->> lines
       parse-graph
       bron-korbosch
       (apply max-key count)
       sort
       (str/join ",")))

;; Everything below here was the result of relaxing assumpations about the problem.
;; They are not correct, but they did end up giving the correct answer for my input.

(defn find-a-maximal-clique
  "Note: does not necessarily return *the* maximal clique that u is a part of"
  [g u v-set]
  (loop [vs (disj v-set u)
         clique #{u}]
    (if-let [v (first vs)]
      (if (= (count (set/intersection clique (g v))) (count clique))
        (recur (rest vs) (conj clique v))
        (recur (rest vs) clique))
      clique)))

(defn part-2-greedy [lines]
  (let [g (parse-graph lines)
        vs (set (keys g))]
    (loop [vs-remaining vs
           mcliques []]
      (if-let [u (first vs-remaining)]
        (let [clique (find-a-maximal-clique g u vs)]
          (recur (set/difference vs-remaining clique)
                 (conj mcliques clique)))
        (->> mcliques
             (apply max-key count)
             sort
             (str/join ","))))))

(defn part-2-greedy2 [lines]
  (let [g (parse-graph lines)
        vs (set (keys g))]
    (->> vs
         (map #(find-a-maximal-clique g % vs))
         (apply max-key count)
         sort
         (str/join ","))))

(comment
  (def test-inp (slurp "inputs/day23.test"))
  (def test-lines (str/split-lines test-inp))

  (def inp (slurp "inputs/day23.inp"))
  (def inp-lines (str/split-lines inp))

  (def g (parse-graph test-lines))
  (count (bron-korbosch g)) ; 36
  (count g)

  (count (clique-3 g)) ; 12
  (count (clique-3 (parse-graph inp-lines))) ; 11011

  (part-1 test-lines) ; 7
  (part-1 inp-lines) ; 1411
  (part-2 test-lines) ; "co,de,ka,ta"
  (time (part-2 inp-lines)) ; "aq,bn,ch,dt,gu,ow,pk,qy,tv,us,yx,zg,zu"

  (part-2-greedy test-lines) ; 4
; "co,de,ka,ta"
  (time (part-2-greedy inp-lines)) ; 13
; "aq,bn,ch,dt,gu,ow,pk,qy,tv,us,yx,zg,zu"

  (part-2-greedy2 test-lines) ; 4
  (time (part-2-greedy2 inp-lines)) ; 13
; "aq,bn,ch,dt,gu,ow,pk,qy,tv,us,yx,zg,zu"
  )

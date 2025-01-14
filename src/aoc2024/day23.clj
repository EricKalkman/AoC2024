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

(defn pairs [col]
  (let [[h & t] col]
    (if (empty? t)
      '()
      (lazy-cat (map #(vector h %) t)
                (pairs (rest col))))))

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

(defn bron-kerbosch-helper
  "Helper function to return the maximum clique of g. Initialize with
  (bron-korbosch-helper g #{} (set (keys g)) #{})."
  [g clique-candidate possible-extensions excluded]
  (if (and (empty? possible-extensions) (empty? excluded))
    [clique-candidate]
    (loop [cliques []
           possible-extensions possible-extensions
           excluded excluded]
      (if-let [v (first possible-extensions)]
        (recur (into 
                        cliques
                        ; find all maximal cliques containing v
                        (bron-kerbosch-helper
                          g
                          ; add v to current candidate clique
                          (conj clique-candidate v)
                          ; narrow possible extensions to only neighbors of v
                          (set/intersection possible-extensions (g v))
                          ; narrow possible exclusions only to neighbors of v
                          (set/intersection excluded (g v))))
               (disj possible-extensions v)
               (conj excluded v))
        cliques))))

(defn bron-kerbosch
  "Returns the maximum clique of g"
  [g]
  (bron-kerbosch-helper g #{} (set (keys g)) #{}))

(defn part-2 [lines]
  (->> lines
       parse-graph
       bron-kerbosch
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
  (count (bron-kerbosch g)) ; 36
  (count g) ; 16

  (count (clique-3 g)) ; 12
  (count (clique-3 (parse-graph inp-lines))) ; 11011

  (def g2 (parse-graph inp-lines))
  (every? #(= 13 (count %)) (vals g2)) ; true ; huh

  (update-vals (->> g2 bron-kerbosch (group-by count)) count) ; {12 78, 2 299, 13 1}

  (def clique (->> g2 bron-kerbosch (apply max-key count)))
  (def outer (into #{} (mapcat #(set/difference (g2 %) clique) clique)))
  (map #(count (set/difference (g2 %) clique)) outer)

  (- 3380 (+ (/ (* 13 12) 2) (/ (* 39 11 10) 2) (* 2 39 11)
             ))

  (->> (range 2 (quot (- (* 78 2) 13) 2))
       (filter #(zero? (mod (- (* 78 2) 13) %)))) ; (11 13)
  (count g2) ; 520
  (/ (reduce + (map count (vals g2))) 2) ; 3380

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

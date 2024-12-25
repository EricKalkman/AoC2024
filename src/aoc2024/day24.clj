(ns aoc2024.day24
  (:require [aoc2024.parsecomb :as p]
            [clojure.string :as str]
            [clojure.set :as set]
            [aoc2024.searches :as search]))

(def wire-name (p/charset+ (set/union p/LETTERS p/DIGITS)))
(def wire (->> (p/p-seq wire-name (p/skip-string ": ") p/p-int)
               (p/label [:name :val])))
(def gate-name (->> (p/p-or (p/string "AND") (p/string "XOR") (p/string "OR"))
                    (p/p-map #(case % "AND" bit-and "XOR" bit-xor "OR" bit-or))))
(def gate (->> (p/p-seq wire-name p/skip-ws gate-name p/skip-ws wire-name
                        (p/skip-string " -> ") wire-name)
               (p/p-map (fn [[a op b out]]
                          [(gensym "gate-") {:args [a b] :op op :out out}]))))

(defn build-graph [gates]
  (->> gates
       (reduce (fn [g [id {:keys [args out]}]]
                 (reduce (fn [g wire]
                           (-> g
                               (update wire #(conj (or % #{}) id))
                               (assoc id #{out})))
                         g
                         args))
               {})))

(defn invert-graph [graph]
  (->> graph
       (reduce
         (fn [g [n es]]
           (reduce
             (fn [g e]
               (update g e #(conj (or % #{}) n)))
             g
             es))
         {})))

(def parse-input (->> (p/p-seq (p/list-+ wire p/nl)
                               p/skip-nl
                               p/skip-nl
                               (p/list-+ gate p/nl))
                      (p/p-map (fn [[wires gates]]
                                 (let [g (build-graph gates)
                                       ig (invert-graph g)]
                                   {:wires wires
                                    :gates (into {} gates)
                                    :graph g
                                    :inv-graph ig})))))

(defn eval-network [wires gates gates-in-order]
  (reduce
    (fn [state node]
      (let [{:keys [op args out]} (gates node)]
        (assoc state out (apply op (map state args)))))
    wires
    gates-in-order))

(defn parse-answer [prefix state]
  (as-> state $
       (keys $)
       (filter #(str/starts-with? % prefix) $)
       (sort (comp - compare) $)
       (map state $)
       (apply str $)
       (Long/parseLong $ 2)))

(defn part-1 [s]
  (let [{:keys [wires graph gates]}
        (->> s p/string->stringbuf parse-input :result)

        [visit _prevs]
        (->> (search/dfs graph (map :name wires) (constantly false)
                         (search/conj-visit [])))
        order (->> (visit) (filter symbol?) reverse)]
    (->> (eval-network (into {} (map (fn [{:keys [name val]}] [name val]))
                             wires)
                       gates order)
         (parse-answer "z"))))

(defn test-bit-helper
  "Returns the set of z wires that don't match the expected z value"
  [evaler n x y nz z]
  (let [start-state
        (into {} (mapcat #(vector [(format "x%02d" %) 0]
                                  [(format "y%02d" %) 0]
                                  [(format "z%02d" %) 0]))
              (range 46))
        nstr (format "%02d" n)
        xid (str "x" nstr)
        yid (str "y" nstr)
        zid (str "z" (format "%02d" nz))
        end-state (evaler (merge start-state {xid x yid y}))
        expected-end-state
        (as-> start-state $
            (assoc $ zid z)
            (into #{} $))]
    (as-> end-state $
         (into #{} (filter #(str/starts-with? (first %) "z")) $)
         (set/difference $ expected-end-state))))

(defn test-bit
  "Returns a vector of [n x y unmatching-z's]"
  [evaler n]
  (loop [x 0
         y 0
         bad []]
    (cond
      (= y 2) (recur (inc x) 0 bad)
      (= x y 1)
      (let [t (into #{} (map first) (test-bit-helper evaler n x y (inc n) 1))]
        (if (empty? t)
          bad
          (conj bad [n x y t])))
      :else
      (let [t (into #{} (map first) (test-bit-helper evaler n x y n (+ x y)))]
        (if (empty? t)
          (recur x (inc y) bad)
          (recur x (inc y) (conj bad [n x y t])))))))

(defn find-bad-bits
  "Returns a map from set of bad z's -> set of bad x/y's"
  [evaler]
  (loop [n 0
         bad []]
    (if (>= n 45)
      (as-> bad $
           (group-by #(nth % 3) $) ; group by the z's that are modified
           (update-vals $ #(into #{} (map first) %))) ; deduplicate x/y
      (let [t (test-bit evaler n)]
        (recur (inc n) (into bad t))))))

(defn find-shared-gates [graph inv-graph start-bits end-bits]
  (let [start-nodes (->> start-bits
                         (mapcat #(vector (str "x" (format "%02d" %))
                                          (str "y" (format "%02d" %)))))
        end-nodes (map #(str "z" (format "%02d" %)) end-bits)
        [v _prevs] (search/dfs graph start-nodes (constantly false)
                                 (search/conj-visit #{}))
        visited-from-top (v)
        
        [v _prevs] (search/dfs inv-graph end-nodes (constantly false)
                               (search/conj-visit #{}))
        visited-from-bottom (v)]
    (->> (set/intersection visited-from-top visited-from-bottom)
         (filter symbol?))))

(defn all-pairs [col]
  (let [[h & t] col]
    (if (empty? t)
      '()
      (lazy-cat (map #(vector h %) t)
                (all-pairs t)))))

(defn all-combinations [lsts]
  (if (empty? (rest lsts))
    (map list (first lsts))
    (let [sub-combos (all-combinations (rest lsts))]
      (apply concat
             (for [x (first lsts)]
               (map #(cons x %) sub-combos))))))

(defn build-gates-with-swap [gates a b]
  (let [a-out (get-in gates [a :out])
        b-out (get-in gates [b :out])]
    (-> gates
        (assoc-in [a :out] b-out)
        (assoc-in [b :out] a-out))))

(defn find-bad-gate-pairs [wires graph inv-graph bad-start-bits bad-end-bits gates]
  (let [shared (find-shared-gates graph inv-graph bad-start-bits bad-end-bits)
        end-bit-names (into #{} (map #(str "z" (format "%02d" %))) bad-end-bits)]
    (->> shared
         all-pairs
         (reduce
           (fn [results [a b]]
             (let [new-gates (build-gates-with-swap gates a b)
                   new-graph (build-graph new-gates)]
               (if-let [[visit _] (search/dfs new-graph (map :name wires) (constantly false)
                                            (search/conj-visit []))]
                 (let [order (->> (visit) (filter symbol?) reverse vec)
                       new-bad-bits (find-bad-bits #(eval-network % new-gates order))]
                   (if (not (new-bad-bits end-bit-names))
                     (conj results [a b])
                     results))
                 results)))
           []))))

(defn random-wire-number [prefix]
  (let [n (long (rand (bit-shift-left 1 45)))]
    (loop [tmp n
          result {}
          i 0]
     (if (>= i 45)
       [n result]
       (recur (bit-shift-right tmp 1)
              (assoc result (str prefix (format "%02d" i)) (bit-and 1 tmp))
              (inc i))))))

(defn gates-swap->evaler [gates wires swap4]
  (let [new-gates (reduce #(apply build-gates-with-swap %1 %2)
                           gates
                           swap4)
        new-graph (build-graph new-gates)
        [visit _] (search/dfs new-graph (map :name wires) (constantly false)
                              (search/conj-visit []))
        order (->> (visit) (filter symbol?) reverse vec)]
    #(eval-network % new-gates order)))

(defn test-random [gates wires swap-options]
  (let [candidates (->> swap-options
                        all-combinations
                        (map #(vector (map (fn [[a b]] [(get-in gates [a :out])
                                                        (get-in gates [b :out])]) %)
                                      (gates-swap->evaler gates wires %))))]
    (loop [candidates candidates]
      (if (== 1 (count candidates))
        (first (first candidates))
        (let [[x x-wires] (random-wire-number "x")
              [y y-wires] (random-wire-number "y")
              initial-state (merge x-wires y-wires)
              sum (+ x y)]
          (println "Sum:" sum (mod sum (bit-shift-left 1 46)))
          (recur
            (->> candidates
                 (filterv (fn [[s runner]]
                            (let [result (->> (runner initial-state)
                                              (parse-answer "z"))]
                              (println result (if (== result sum) "GOOD" "BAD")
                                       s)
                              (== result sum)))))))))))

(defn part-2 [s]
  (let [{:keys [wires graph gates inv-graph]}
        (->> s p/string->stringbuf parse-input :result)

        [visit _prevs]
        (->> (search/dfs graph (map :name wires) (constantly false)
                         (search/conj-visit [])))
        order (->> (visit) (filter symbol?) reverse vec)
        
        bad-bits (find-bad-bits #(eval-network % gates order))]
    (assert (= 4 (count bad-bits)) "did not find exactly 4 sections with bad bits")
    (->> bad-bits
         (pmap (fn [[bad-ends bad-starts]]
                (let [bad-ends (into #{} (map #(parse-long (subs % 1)) bad-ends))]
                  (find-bad-gate-pairs wires graph inv-graph
                                      bad-starts
                                      bad-ends gates))))
         (test-random gates wires)
         (apply concat)
         sort
         (str/join ","))))

(comment
  (def test-inp (str/trim (slurp "inputs/day24.test0")))
  (def p (->> test-inp p/string->stringbuf parse-input :result))

  (def test-inp-2 (str/trim (slurp "inputs/day24.test")))
  (def p2 (->> test-inp-2 p/string->stringbuf parse-input :result))

  (def inp (str/trim (slurp "inputs/day24.inp")))
  (def pinp (->> inp p/string->stringbuf parse-input :result))
  (->> (:wires pinp) (map :name) (filter #(str/starts-with? % "x")) count) ; 45
  (->> (:wires pinp) (map :name) (filter #(str/starts-with? % "y")) count) ; 45
  (->> (:graph pinp) vals (apply concat) (filter #(str/starts-with? % "z")) count) ; 46

  (part-2 inp) ; "cph,jqn,kwb,qkf,tgr,z12,z16,z24"


  ; back from brute force
; ("cph,jqn,qkf,tgr,vjq,wwd,z12,z16"
;  "cph,jqn,qkf,tgr,vjq,z12,z16,z24"
;  "cph,jqn,kwb,qkf,tgr,wwd,z12,z16"
;  "cph,jqn,kwb,qkf,tgr,z12,z16,z24")  <-- this one's the answer

; ([[gate-20871 "kwb" gate-21029 "z12"]
;   [gate-20965 "vjq" gate-21029 "z12"]]
;  [[gate-20978 "qkf" gate-21061 "z16"]]
;  [[gate-21064 "tgr" gate-20916 "z24"]
;   [gate-21064 "tgr" gate-21019 "wwd"]]
;  [[gate-20953 "cph" gate-20912 "jqn"]])

; {#{"z12" "z13"} #{12 11},
;  #{"z16" "z17"} #{15 16},
;  #{"z24" "z25"} #{24 23},
;  #{"z30" "z29"} #{29}}


; test-bit
; {#{"z12" "z13"}
;  [[11 1 1 #{"z12" "z13"}]
;   [12 0 1 #{"z12" "z13"}]
;   [12 1 0 #{"z12" "z13"}]
;   [12 1 1 #{"z12" "z13"}]],
;  #{"z16" "z17"}
;  [[15 1 1 #{"z16" "z17"}]
;   [16 0 1 #{"z16" "z17"}]
;   [16 1 0 #{"z16" "z17"}]],
;  #{"z24" "z25"}
;  [[23 1 1 #{"z24" "z25"}]
;   [24 0 1 #{"z24" "z25"}]
;   [24 1 0 #{"z24" "z25"}]
;   [24 1 1 #{"z24" "z25"}]],
;  #{"z30" "z29"}
;  [[29 0 1 #{"z30" "z29"}]
;   [29 1 0 #{"z30" "z29"}]
;   [29 1 1 #{"z30" "z29"}]]}

; {#{"z12" "z13"} #{12 11},
;  #{"z16" "z17"} #{15 16},
;  #{"z24" "z25"} #{24 23},
;  #{"z30" "z29"} #{29}}

  (part-1 test-inp) ; 4
  (part-1 test-inp-2) ; 2024
  (part-1 inp) ; 52038112429798
  )

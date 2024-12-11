(ns aoc2024.searches
  (:import [java.util ArrayDeque]))

(defn nil-visit
  ([] nil)
  ([_ _] nil-visit))

(defn conj-visit [init-state]
  (fn
    ([] init-state)
    ([_prevs next] (conj-visit (conj init-state next)))))

(defn update-visit [f state]
  (fn
    ([] state)
    ([_prevs cur] (update-visit f
                                (update state cur #(f % cur))))))

(defn bfs
  ([neighfunc src stop?] (bfs neighfunc src stop? nil-visit))
  ([neighfunc src stop? visit]
  (loop [q (doto (new ArrayDeque 32) (.push src))
         prevs {src src}
         visit visit]
    (if-let [cur (and (not (.isEmpty q)) (.pop q))]
      (if (stop? cur)
        {:prevs prevs :last cur :visit (visit cur)}
        (let [neighs (->> (neighfunc cur) (filterv (complement prevs)))
              new-prevs (reduce #(assoc %1 %2 cur) prevs neighs)]
          (recur (reduce #(doto %1 (.addLast %2)) q neighs)
                 new-prevs
                 (visit prevs cur))))
      {:prevs prevs :visit visit}))))

(comment
  (def g {:a #{:b :c} :b #{:d :e} :c #{:f :g}})

  )

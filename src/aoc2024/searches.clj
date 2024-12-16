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

(defn dijkstra [neighfunc srcs stop?]
  (loop [costs (into (sorted-set) (map #(vector 0 %)) srcs)
         visited? #{}
         prevs {}]
    (if-let [[n1-cost n1 :as nc] (first costs)]
      (cond
        (stop? n1) {:last n1 :cost n1-cost :prevs prevs}
        (visited? n1) (recur (disj costs nc) visited? prevs)
        :else
        (let [[costs prevs]
              (->> (neighfunc n1)
                   (reduce
                     (fn [[costs prevs] [n2 edge-cost]]
                       (let [new-n2-cost (+ n1-cost edge-cost)]
                         (if-let [[prev-cost _] (first (prevs n2))]
                           (if (< prev-cost new-n2-cost)
                             [costs prevs]
                             [(conj costs [new-n2-cost n2])
                              (update prevs n2 #(conj (or % #{}) [new-n2-cost n1]))])
                           [(conj costs [new-n2-cost n2]) (assoc prevs n2 #{[new-n2-cost n1]})])))
                     [(disj costs nc) prevs]))]
          (recur costs (conj visited? n1) prevs)))
      {:prevs prevs})))

(comment
  (def g {:a #{:b :c} :b #{:d :e} :c #{:f :g}})

  (def g2 {:a [[:b 1] [:d 10]] :b [[:c 1]] :c [[:d 1]] :d [[:e 1]]})

  (dijkstra g2 [:a] #(= % :d))


  )

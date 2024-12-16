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
         prevs {}
         lasts nil]
    (if-let [[n1-cost n1 :as nc] (first costs)]
      (cond
        ; accumulate all nodes in a row that satisfy the stop cond
        (stop? n1) (recur (disj costs nc) visited? prevs (conj (or lasts []) nc))
        ; lasts /= nil indicates stop condition has been reached; collect all nodes of current cost
        ; that satisfy stop?, discarding nodes of the same cost that don't
        (and lasts (== n1-cost (first (first lasts)))) (recur (disj costs nc) visited? prevs lasts)
        lasts {:lasts (map second lasts) :prevs prevs :cost (first (first lasts))}
        (visited? n1) (recur (disj costs nc) visited? prevs lasts)
        :else
        (let [[costs prevs]
              (->> (neighfunc n1)
                   (reduce
                     (fn [[costs prevs] [n2 edge-cost]]
                       (let [new-n2-cost (+ n1-cost edge-cost)]
                         (if-let [[prev-cost _] (first (prevs n2))]
                           (cond
                             (< prev-cost new-n2-cost) [costs prevs]
                             (== prev-cost new-n2-cost)
                             [costs
                              (update prevs n2 #(conj (or % #{}) [new-n2-cost n1]))]
                             :else [(conj costs [new-n2-cost n2])
                                    (assoc prevs n2 #{[new-n2-cost n1]})])
                           [(conj costs [new-n2-cost n2]) (assoc prevs n2 #{[new-n2-cost n1]})])))
                     [(disj costs nc) prevs]))]
          (recur costs (conj visited? n1) prevs lasts)))
      (assoc {:prevs prevs :lasts lasts}
             :cost
             (and lasts (first (first lasts)))))))

(comment
  (def g {:a #{:b :c} :b #{:d :e} :c #{:f :g}})

  (def g2 {:a [[:b 1] [:d 10]] :b [[:c 1]] :c [[:d 1]] :d [[:e 1]]})

  (dijkstra g2 [:a] #(= % :d))


  )

(ns aoc2024.searches
  (:require [clojure.data.priority-map :refer [priority-map]])
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
  ([neighfunc src stop?] (bfs neighfunc src stop? nil-visit identity))
  ([neighfunc src stop? visit] (bfs neighfunc src stop? visit identity))
  ([neighfunc src stop? visit node-keyfn]
  (loop [q (doto (new ArrayDeque 32) (.push src))
         prevs (transient {(node-keyfn src) src})
         visit visit]
    (if-let [cur (and (not (.isEmpty q)) (.pop q))]
      (if (stop? cur)
        {:prevs (persistent! prevs) :last cur :visit (visit prevs cur)}
        (let [neighs (->> (neighfunc cur) (filterv (comp (complement prevs) node-keyfn)))
              new-prevs (reduce #(assoc! %1 (node-keyfn %2) cur) prevs neighs)]
          (recur (reduce #(doto %1 (.addLast %2)) q neighs)
                 new-prevs
                 (visit prevs cur))))
      {:prevs (persistent! prevs) :visit visit}))))


(defn update! [mt k f]
  (let [cur (mt k)]
    (assoc! mt k (f cur))))

(defrecord NodeCost [cost nodes])

(defn dijkstra [neighfunc srcs stop?]
  (loop [costs (into (priority-map) (map #(vector % 0)) srcs)
         visited? (transient #{})
         prevs (transient {})
         lasts nil]
    (if-let [[n1 n1-cost :as nc] (peek costs)]
      (cond
        ; accumulate all nodes that satisfy the stop cond
        (stop? n1) (recur (pop costs) visited? prevs (conj (or lasts #{}) nc))
        ; lasts /= nil indicates stop condition has been reached; collect all nodes of current cost
        ; that satisfy stop?, discarding nodes of the same cost that don't
        (and lasts (== n1-cost (second (first lasts)))) (recur (pop costs) visited? prevs lasts)
        lasts {:lasts (map first lasts) :prevs (persistent! prevs) :cost (second (first lasts))}
        (visited? n1) (recur (pop costs) visited? prevs lasts)
        :else
        (let [[costs prevs]
              (->> (neighfunc n1)
                   (reduce
                     (fn [[costs prevs] [n2 edge-cost]]
                       (let [new-n2-cost (+ n1-cost edge-cost)]
                         (if-let [^NodeCost prev (prevs n2)]
                           (cond
                             (< (.cost prev) new-n2-cost) [costs prevs]
                             (== (.cost prev) new-n2-cost)
                             [costs
                              (update! prevs n2 (fn [^NodeCost nc] (update nc :nodes (fn [set] (conj set n1)))))]
                             :else [(assoc costs n2 new-n2-cost)
                                    (assoc! prevs n2 (->NodeCost new-n2-cost #{n1}))])
                           [(assoc costs n2 new-n2-cost)
                            (assoc! prevs n2 (->NodeCost new-n2-cost #{n1}))])))
                     [(pop costs) prevs]))]
          (recur costs (conj! visited? n1) prevs lasts)))
      (assoc {:prevs (persistent! prevs) :lasts lasts}
             :cost
             (and lasts (second (first lasts)))))))

(defn- dfs-helper [neighfunc src stop? visit visiting? prevs]
  (if (stop? src)
    [(visit prevs src) visiting? prevs]
    (let [neighs (neighfunc src)]
      (fn []
        (if-let [res
                 (->> neighs
                      (reduce
                        (fn [[visit visiting? prevs] neigh]
                          (cond
                            (visiting? neigh) (reduced nil) ; cycle detected
                            (prevs neigh) [visit visiting? prevs]
                            :else
                            (or (trampoline dfs-helper neighfunc neigh stop?
                                            visit visiting? (assoc prevs neigh src))
                                (reduced nil))))
                        [visit (conj visiting? src) prevs]))]
          (let [[visit visiting? prevs] res]
            [(visit prevs src) (disj visiting? src) prevs])
          nil)))))

(defn dfs
  ([neighfunc srcs stop?] (dfs neighfunc srcs stop? nil-visit))
  ([neighfunc srcs stop? visit]
   (if-let [res (->> srcs
                     (reduce
                       (fn [[visit visiting? prevs] src]
                         (or (trampoline dfs-helper neighfunc src stop? visit visiting? prevs)
                             (reduced nil)))
                       [visit #{} {}]))]
     res
     nil)))

(comment
  (def g {:a #{:e :b :c} :b #{:d :e} :c #{:f :g}})

  ((first (dfs g [:a] (constantly false) (conj-visit [])))) ; [:e :g :f :c :d :b :a]

  (def g2 {:a [[:b 1] [:d 10]] :b [[:c 1]] :c [[:d 1]] :d [[:e 1]]})

  (dijkstra g2 [:a] #(= % :d))


  )

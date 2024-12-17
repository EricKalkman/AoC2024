(ns aoc2024.day09
  (:require [clojure.string :as s]))

(defn parse-input [s]
  (let [xs (->> (str s \0) (map (comp parse-long str)))]
    (->> xs
         (map #(hash-map :id (/ %1 2) :pos %2 :len %3)
              (range) (reductions + 0 xs))
         (partition 2)
         (apply map vector)
         (zipmap [:files :spaces]))))

(defn move-blocks-to-start [{:keys [files spaces]}]
  (loop [files files
         spaces spaces
         res []]
    (cond
      (empty? files) (assert false "there should always be a starting file remaining")
      (or (empty? spaces)
          (> (:pos (first spaces)) (:pos (peek files)))) (concat res files)
      (zero? (:len (first spaces))) (recur files (rest spaces) res)
      (zero? (:len (peek files))) (recur (pop files) spaces res)
      :else
      (let [{s-id :id s-len :len s-pos :pos} (first spaces)
            {f-id :id f-len :len f-pos :pos} (peek files)
            to-consume (min s-len f-len)]
        (recur (conj (pop files) {:id f-id :len (- f-len to-consume) :pos f-pos})
               (cons {:id s-id :len (- s-len to-consume) :pos (+ s-pos to-consume)} (rest spaces))
               (conj res {:id f-id :len to-consume :pos s-pos}))))))

(defn triangle-sum [first last]
  (/ (* (inc (- last first)) (+ first last)) 2)) ; hey look it's Gauss

(defn checksum [blocks]
  (->> blocks
       (map #(* (:id %) (triangle-sum (:pos %) (+ (:pos %) (:len %) -1))))
       (reduce +)))

(defn part [mover s]
  (->> s
       s/trim     ; guess how I realized that I needed to do this?
       parse-input
       mover
       checksum))

(def part-1 (partial part move-blocks-to-start))

(defn move-files-to-start [{:keys [files spaces]}]
  (loop [files files
         spaces spaces
         res []]
    (if (empty? files)
      res
      (let [file (peek files)
            [l r] (split-with #(and (< (:pos %) (:pos file)) (< (:len %) (:len file))) spaces)]
        (cond
          (empty? r) (recur (pop files) spaces (conj res file))
          (>= (:pos (first r)) (:pos file)) (recur (pop files) l (conj res file))
          :else
          (let [{s-len :len s-pos :pos} (first r)]
            (recur (pop files)
                   (concat l
                           (if (= s-len (:len file))
                             (rest r)
                             (cons {:pos (+ s-pos (:len file)) :len (- s-len (:len file))}
                                   (rest r))))
                   (conj res (assoc file :pos s-pos)))))))))

(defn move-files-to-start-faster [{:keys [files spaces]}]
  (loop [files files
         spaces (->> spaces
                     (group-by :len)
                     (map (fn [[len spaces]]
                            [len (into (sorted-set-by #(compare (:pos %1) (:pos %2)))
                                       spaces)]))
                     (into {}))
         res []]
    (if-let [file (peek files)]
      (if-let [possible-lens (seq (filter #(and (seq (spaces %))
                                                (< (:pos (first (spaces %))) (:pos file)))
                                          (range (:len file) 10)))]
        (let [earliest-len (apply min-key (comp :pos first spaces) possible-lens)
              {s-pos :pos s-len :len :as space} (first (spaces earliest-len))]
          (recur (pop files)
                 (-> spaces
                     (update s-len #(disj % space))
                     (update (- s-len (:len file))
                             #(conj % {:pos (+ s-pos (:len file)) :len (- s-len (:len file))})))
                 (conj res (assoc file :pos s-pos))))
        (recur (pop files) spaces (conj res file)))
      res)))

(def part-2-old (partial part move-files-to-start))

(def part-2 (partial part move-files-to-start-faster))

(comment
  (part-1 (slurp "inputs/day09.inp")) ; 6310675819476

  (part-2-old (slurp "inputs/day09.inp")) ; 6335972980679
  (part-2 (slurp "inputs/day09.inp")) ; 6335972980679

  (part-2 (slurp "inputs/day09_bonus.inp")) ; 97898222299196
  (part-2 (slurp "inputs/day09_really_evil.inp")) ; 5799706413896802
  )

(ns aoc2024.day22
  (:require [clojure.string :as str]))

(def dec-modulus (dec 16777216))
; (bit-shift-left 1 24) ; 16777216

(defn next-secret [^long n] ^long
  (let [a (-> n (bit-shift-left 6) (bit-xor n) (bit-and dec-modulus))
        b (-> a (bit-shift-right 5) (bit-xor a) (bit-and dec-modulus))
        c (-> b (bit-shift-left 11) (bit-xor b) (bit-and dec-modulus))]
    c))

; a = ((n << 6) xor n) and (dec (1 << 24))
; b = ((a >>> 5) xor a) and (dec (1 << 24))
; c = ((b << 11) xor b) and (dec (1 << 24))

(defn part-1 [lines]
  (->> lines
       (map parse-long)
       (map #(as-> % $ (iterate next-secret $) (nth $ 2000)))
       (reduce +)))

(defn hash-quartet [^long a ^long b ^long c ^long d]
  (+ (+ 9 d)
     (* 20 (+ (+ 9 c)
              (* 20 (+ (+ 9 b) (* 20 (+ 0 a))))))))

(defn part-2-secrets-mod-10
  "Returns seq of [diff, price at end of diff] for 2000 new secrets"
  [^long s]
  ^clojure.lang.PersistentVector
  (loop [idx 2000
         s s
         prev-mod (mod s 10)
         result [[(mod s 10) (mod s 10)]]]
    (if (zero? idx)
      result
      (let [n (next-secret s)
            m (mod n 10)]
        (recur (dec idx)
               n
               m
               (conj result [(- m prev-mod) m]))))))

(defn part-2 [lines]
  (->> lines
       (pmap #(->> %
                   parse-long
                   part-2-secrets-mod-10
                   (partition 4 1)
                   (reduce
                     (fn [seq-to-winnings ps]
                       (let [v (vec ps)
                             k (hash-quartet ^long (get (get v 0) 0)
                                             ^long (get (get v 1) 0)
                                             ^long (get (get v 2) 0)
                                             ^long (get (get v 3) 0))
                             n ^long (get (peek v) 1)]
                         (update seq-to-winnings
                                 k
                                 (fn [n*] ^long (or n* n)))))
                     {})))
       (reduce #(merge-with + %1 %2) {})
       (reduce (fn [m [_ v]] (max m v)) 0)))

(comment
  (def test-inp (slurp "inputs/day22.test1"))
  (def test-inp-2 (slurp "inputs/day22.test2"))


  (def test-lines (str/split-lines test-inp))
  (def test-lines-2 (str/split-lines test-inp-2))

  (part-1 test-lines) ; 37327623
  (part-1 (str/split-lines (slurp "inputs/day22.inp"))) ; 14622549304

  (part-2 test-lines-2) ; ["-21-13" 23]
  (time (part-2 (str/split-lines (slurp "inputs/day22.inp")))) ; ["0-220" 1735]

  )

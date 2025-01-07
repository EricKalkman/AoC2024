(ns aoc2024.day17
  (:require [clojure.string :as str]
            [aoc2024.parsecomb :as p])
  (:import [clojure.lang PersistentVector]))

(defrecord State [ip a b c output])

(def opcode->op [:adv :bxl :bst :jnz :bxc :out :bdv :cdv])

(def parse-input
  (->> (p/p-seq (p/skip-string "Register A: ") p/p-int p/skip-nl
                (p/skip-string "Register B: ") p/p-int p/skip-nl
                (p/skip-string "Register C: ") p/p-int p/skip-nl
                p/skip-nl
                (p/skip-string "Program: ") (p/list-+ p/p-int (p/chr \,) :vectorize? true))
       (p/p-map #(hash-map :commands (last %)
                           :state (->State 0 (first %) (second %) (nth % 2) [])))))

(defn combo-val [^State p ^long x]
  (cond
    (<= 0 x 3) x
    (== x 4) (.a p)
    (== x 5) (.b p)
    (== x 6) (.c p)))

(defn run-op [^State state ^long op ^long x]
  (case (opcode->op op)
    :adv (assoc state :a (bit-shift-right (.a state) (combo-val state x)))
    :bxl (update state :b #(bit-xor % x))
    :bst (assoc state :b (bit-and (combo-val state x) 7))
    :jnz (if (zero? (.a state))
           state
           (assoc state :ip (- x 2)))
    :bxc (update state :b #(bit-xor % (.c state)))
    :out (update state :output #(conj % (bit-and (combo-val state x) 7)))
    :bdv (assoc state :b (bit-shift-right (.a state) (combo-val state x)))
    :cdv (assoc state :c (bit-shift-right (.a state) (combo-val state x)))
    (assert false (str "unrecognized operation " op))))

(defn run-program [initial-state cmds]
  (loop [{:keys [^long ip] :as state} initial-state]
    (if (>= ip (count cmds))
      state
      (let [op (.nth ^PersistentVector cmds ip)
            x (.nth ^PersistentVector cmds (inc ip))]
        (-> state
            (run-op op x)
            (update :ip #(+ % 2))
            recur)))))

(defn part-1 [s]
  (let [{:keys [commands state]}
        (->> s
             p/string->stringbuf
             parse-input
             :result)]
    (->> (run-program state commands)
         :output
         (str/join \,))))

(defn a->bits [a]
  (loop [s ""
         a a]
    (if (zero? a)
      s
      (let [lo (bit-and a 7)
            hi (bit-shift-right a 3)]
        (recur (str (Integer/toString lo 2) "_" s)
               hi)))))

(defn solve-quinoid [commands]
  (let [len (count commands)]
    (loop [a 0
           cur-word 0
           word-stack []
           ridx 0]
      (cond
        (>= ridx len)
        ; right shift needed to undo the last left shift that would've made room
        ; for another digit
        [(bit-shift-right a 3)
         (:output (run-program (->State 0 (bit-shift-right a 3) 0 0 []) commands))]

        ; if we've scanned all possible values of this word, backtrack
        ; note: will fail hard due to pop on empty vec if there is no solution
        (>= cur-word (bit-shift-left 1 3))
        (recur (bit-and-not (bit-shift-right a 3) 7)
               (inc (peek word-stack)) (pop word-stack) (dec ridx))

        :else
        (let [a-to-run (bit-or a cur-word)
              res (.output ^State (run-program (->State 0 a-to-run 0 0 []) commands))]
          (if (= (.nth ^PersistentVector res 0) (.nth ^PersistentVector commands (- len ridx 1)))
            (recur (bit-shift-left a-to-run 3) 0 (conj word-stack cur-word) (inc ridx))
            (recur a (inc cur-word) word-stack ridx)))))))

(defn solve-quinoid-recursive [commands idx a]
  (if (< idx 0)
    a
    (->> (range (bit-shift-left 1 3))
         (map #(let [new-a (bit-or % (bit-shift-left a 3))]
                 (-> (->State 0 new-a 0 0 [])
                     (run-program commands)
                     :output
                     (vector new-a))))
         (some (fn [[output new-a]]
                 (and (= (.nth ^PersistentVector commands idx)
                         (.nth ^PersistentVector output 0))
                      (solve-quinoid-recursive commands (dec idx) new-a)))))))

(defn part-2 [s]
  (->> s
       p/string->stringbuf
       parse-input
       :result
       :commands
       solve-quinoid
       first))

(defn part-2-recursive [s]
  (let [cmds (->> s
                  p/string->stringbuf
                  parse-input
                  :result
                  :commands)]
    (solve-quinoid-recursive cmds (dec (count cmds)) 0)))

(comment
  (part-1 (slurp "inputs/day17.inp")) ; "5,1,4,0,5,1,0,2,6"

  (part-2 (slurp "inputs/day17.inp"))           ; 202322936867370
  (part-2-recursive (slurp "inputs/day17.inp")) ; 202322936867370

  (def reddit-inp "Register A: 12345678
Register B: 0
Register C: 0

Program: 2,4,1,0,7,5,1,5,0,3,4,5,5,5,3,0")

  (part-1 reddit-inp) ; "6,0,4,5,4,5,2,0"
  (part-2 reddit-inp) ; 202797954918051
  (part-2-recursive reddit-inp) ; 202797954918051
  (a->bits 202797954918051) ; "101_110_0_111_0_110_10_100_0_100_0_110_11_10_100_11_"

  )

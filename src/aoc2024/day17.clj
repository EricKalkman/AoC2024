(ns aoc2024.day17
  (:require [clojure.string :as str]
            [aoc2024.parsecomb :as p]))

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
      (let [op (.nth cmds ip)
            x (.nth cmds (inc ip))]
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

(defn solve [commands]
  (let [len (count commands)]
    (loop [a 1
           ridx 0]
      (if (>= ridx len)
        [(bit-shift-right a 3)
         (:output (run-program (->State 0 (bit-shift-right a 3) 0 0 []) commands))]
        (let [res (:output (run-program (->State 0 a 0 0 []) commands))]
          (cond
            (== (.nth res (- (count res) ridx 1)) (.nth commands (- len ridx 1)))
            (recur (bit-shift-left a 3)
                   (inc ridx))
            :else
            (recur (inc a) ridx)))))))

(defn part-2 [s]
  (->> s
       p/string->stringbuf
       parse-input
       :result
       :commands
       solve
       first))

(comment
  (part-1 (slurp "inputs/day17.inp")) ; "5,1,4,0,5,1,0,2,6"

  (def real-p (->> (slurp "inputs/day17.inp") p/string->stringbuf parse-input :result))

  (part-2 (slurp "inputs/day17.inp")) ; 202322936867370
  )

(library (day07)
         (export parse-numbers parse-input part-1 part-2)
         (import (rnrs)
                 (rnrs r5rs)
                 (util)
                 (prefix (parsecomb) p:))

(define parse-numbers (->> (p:seq p:parse-int (p:skip (p:str ": "))
                                  (p:list-of p:parse-int (p:char #\space)))
                           (p:label '(target terms))))
(define parse-input (p:list-of parse-numbers p:nl))

(define (deconcat target x)
  (define divisor (exact (round (expt 10 (ceiling (log (+ x 1) 10))))))
  (let-values ([(d r) (div-and-mod target divisor)])
    (and (= r x) d)))

(define (inverse op target x)
  (case op
    [(+) (- target x)]
    [(*) (/ target x)]
    [(cat) (deconcat target x)]))

(define (thread-print x) (display x) (newline) x)
(define (determine-ops target terms ops)
  (cond
    [(and (null? (cdr terms)) (= (car terms) target)) (list (cons 'I target))]
    [(null? (cdr terms)) #f]
    [else
      (->> ops
           (exists
             (λ (op)
                (let* ([next-term (car terms)]
                       [next-target (inverse op target next-term)])
                  (and-> next-target
                         (determine-ops (cdr terms) ops)
                         (rcons (cons op next-term)))))))]))

(define (part ops s)
  (define eqs (->> s (p:make-string-buffer) (parse-input) (p:parse-result-unwrap)))
  (->> eqs
       (filter (λ (eq)
                    (determine-ops (assv-get 'target eq)
                                   (reverse (assv-get 'terms eq))
                                   ops)))
       (map (λ (eq) (assv-get 'target eq)))
       (apply +)))

(define part-1 (partial part '(+ *)))
(define part-2 (partial part '(+ * cat)))
)

#|
(import (rnrs) (parsecomb) (util) (day07))


(define test-inp "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(part-1 test-inp) ; 3749
(part-1 (file->string "../inputs/day07.inp")) ; 66343330034722

(part-2 test-inp) ; 11387
(part-2 (file->string "../inputs/day07.inp")) ; 637696070419031
|#

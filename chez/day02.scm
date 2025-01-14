#!r6rs

(library (day02)
         (export part-1 part-2)

         (import (rnrs)
                 (util)
                 (prefix (parsecomb) p:))

  (define line-parser (p:list-of p:parse-int p:ws))
  (define parse-line (compose p:parse-result-val line-parser p:make-string-buffer))

  (define (report-safe? report)
    (define fst (- (car report) (cadr report)))
    (->> (map* - report (cdr report))
         (for-all (λ (diff) (and (> (* fst diff) 0)
                                 (<= 1 (abs diff) 3))))))

  (define (part-1 lines)
    (->> lines
         (fold-left
           (λ (sum line)
              (if (report-safe? (parse-line line)) (+ sum 1) sum))
           0)))

  (define (range n)
    (let loop ([n (- n 1)]
               [res '()])
      (if (< n 0)
        res
        (loop (- n 1)
              (cons n res)))))

  (define (skip-nth n lst)
    (cond
      [(null? lst) lst]
      [(zero? n) (cdr lst)]
      [else (cons (car lst) (skip-nth (- n 1) (cdr lst)))]))

  (define (part-2 lines)
    (->> lines
         (map parse-line)
         (count (λ (report)
                   (exists (λ (n) (report-safe? (skip-nth n report)))
                           (range (length report)))))))
)

#|
(import (day02))
(import (util))

(define lines (string->lines "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"))

(part-1 lines) ; 2
(part-1 (file->lines "../inputs/day02.inp")) ; 663

(part-2 lines) ; 4
(part-2 (file->lines "../inputs/day02.inp")) ; 692
|#

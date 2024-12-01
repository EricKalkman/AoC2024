#!r6rs

(library (day01)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (prefix (parsecomb) p:))

  (define line-parser (p:seq p:parse-int p:skip-ws p:parse-int))
  (define parse-line (compose p:parse-result-val line-parser p:make-string-buffer))

  (define (part-1 lines)
    (->> lines
         (map parse-line)
         (apply map (compose (λ (x) (list-sort < x)) list))
         (apply map (compose abs -))
         (fold-left + 0)))

  (define (assv-get x lst) ; assv returns the cons cell; assv-get cdr's the cell
    (and->> (assv x lst)
            (cdr)))

  (define (part-2 lines)
    (let ([lsts (->> lines
                     (map parse-line)
                     (apply map list))])
      (as-> (cadr lsts) x
            (fold-left (λ (table n)
                          (acons n (+ 1 (or (assv-get n table) 0)) table))
                       '() x)
            (map (λ (n) (* n (or (assv-get n x) 0))) (car lsts))
            (fold-left + 0 x))))
)

#|
(import (day01) (util))
(define test-inp "3   4
4   3
2   5
1   3
3   9
3   3")

(part-1 (string->lines test-inp)) ; 11
(part-2 (string->lines test-inp)) ; 31

(part-1 (file->lines "../inputs/day01.inp")) ; 2176849
(part-2 (file->lines "../inputs/day01.inp")) ; 23384288
|#

(library (day11)
         (export part-1 part-2 split-int)
         (import (rnrs)
                 (util))

  (define (parse-input s)
    (call-with-port (open-string-input-port s)
      (λ (p)
         (let loop ([res '()])
           (let ([x (read p)])
             (if (eof-object? x)
               res
               (loop (cons x res))))))))

  (define (tally-into h v to-add)
    ((if (vector? v) vector-fold-left fold-left)
      (λ (h k)
         (mut-> h (hashtable-update! k (λ (n) (+ n to-add)) 0)))
      h v))

  (define (split-int n)
    (define num-digits (exact (ceiling (log (+ n 1) 10))))
    (if (even? num-digits)
      (let ([divisor (exact (round (expt 10.0 (/ num-digits 2.0))))])
        (let-values ([(upper lower) (div-and-mod n divisor)])
          (list upper lower)))
      #f))

  (define (transition stone)
    (if (zero? stone)
      (list 1)
      (if-let ([split (split-int stone)])
        split
        (list (* 2024 stone)))))

  (define (blink stones)
    (let-values ([(stones counts) (hashtable-entries stones)])
      (vector-fold-left
        (λ (h stone count)
           (tally-into h (transition stone) count))
        (make-eqv-hashtable)
        stones counts)))

  (define (part n s)
    (let loop ([n n]
               [stones (tally-into (make-eqv-hashtable) (parse-input s) 1)])
      (if (zero? n)
        (let-values ([(_ counts) (hashtable-entries stones)])
          (vector-fold-left + 0 counts))
        (loop (- n 1)
              (blink stones)))))

  (define part-1 (partial part 25))
  (define part-2 (partial part 75))
)

#|
(import (rnrs) (util) (day11))

(part-1 "125 17") ; 55312
(part-1 (file->string "../inputs/day11.inp")) ; 185205
(part-2 "125 17") ; 65601038650482
(part-2 (file->string "../inputs/day11.inp")) ; 221280540398419
|#

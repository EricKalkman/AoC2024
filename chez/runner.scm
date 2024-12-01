(import (rnrs)
        (util)
        (prefix (day01) day01:))

(define (do-part day p1 p2)
  (format #t "DAY ~a~%" day)
  (format #t "Part 1: ~2,'0d~%" (time-exec (p1)))
  (format #t "Part 2: ~2,'0d~%" (time-exec (p2)))
  (newline))

(define (line-thunks day p1 p2)
  (if-let ([lines (file->lines (format "../inputs/day~2,'0d.inp" day))])
    (list (λ () (p1 lines)) (λ () (p2 lines)))
    #f))

(and->> (line-thunks 1 day01:part-1 day01:part-2) (apply do-part 1))

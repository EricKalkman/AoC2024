(import (rnrs)
        (util)
        (prefix (day01) day01:)
        (prefix (day02) day02:)
        (prefix (day03) day03:))

(define (do-part day p1 p2)
  (format #t "DAY ~a~%" day)
  (format #t "Part 1: ~2,'0d~%" (time-exec (p1)))
  (format #t "Part 2: ~2,'0d~%" (time-exec (p2)))
  (newline))

(define (line-thunks day p1 p2)
  (let ([lines (file->lines (format "../inputs/day~2,'0d.inp" day))])
    (list (λ () (p1 lines)) (λ () (p2 lines)))))

(define (str-thunks day p1 p2)
  (let ([s (file->string (format "../inputs/day~2,'0d.inp" day))])
    (list (λ () (p1 s)) (λ () (p2 s)))))

(and->> (line-thunks 1 day01:part-1 day01:part-2) (apply do-part 1))
(and->> (line-thunks 2 day02:part-1 day02:part-2) (apply do-part 2))
(and->> (str-thunks 3 day03:part-1 day03:part-2) (apply do-part 3))

(import (rnrs)
        (util)
        (prefix (day01) day01:)
        (prefix (day02) day02:)
        (prefix (day03) day03:)
        (prefix (day04) day04:)
        (prefix (day05) day05:)
        (prefix (day06) day06:)
        (prefix (day07) day07:)
        (prefix (day08) day08:)
        (prefix (day09) day09:)
        (prefix (day10) day10:)
        (prefix (day11) day11:)
        (prefix (day18) day18:)
        (prefix (day19) day19:)
        )

(define (do-part day p1 p2)
  (format #t "DAY ~a~%" day)
  (format #t "Part 1: ~a~%" (time-exec (p1)))
  (format #t "Part 2: ~a~%" (time-exec (p2)))
  (newline))

(define (line-thunks day p1 p2)
  (let ([lines (file->lines (format #f "../inputs/day~2,'0d.inp" day))])
    (list (λ () (p1 lines)) (λ () (p2 lines)))))

(define (str-thunks day p1 p2)
  (let ([s (file->string (format #f "../inputs/day~2,'0d.inp" day))])
    (list (λ () (p1 s)) (λ () (p2 s)))))

(and->> (line-thunks 1 day01:part-1 day01:part-2) (apply do-part 1))
(and->> (line-thunks 2 day02:part-1 day02:part-2) (apply do-part 2))
(and->> (str-thunks 3 day03:part-1 day03:part-2) (apply do-part 3))
(and->> (str-thunks 4 day04:part-1 day04:part-2) (apply do-part 4))
(and->> (str-thunks 5 day05:part-1 day05:part-2) (apply do-part 5))
(and->> (str-thunks 6 day06:part-1 day06:part-2) (apply do-part 6))
(and->> (str-thunks 7 day07:part-1 day07:part-2) (apply do-part 7))
(and->> (str-thunks 8 day08:part-1 day08:part-2) (apply do-part 8))
(and->> (str-thunks 9 day09:part-1 day09:part-2) (apply do-part 9))
(and->> (str-thunks 10 day10:part-1 day10:part-2) (apply do-part 10))
(and->> (str-thunks 11 day11:part-1 day11:part-2) (apply do-part 11))
(and->> (str-thunks 18 day18:part-1 day18:part-2) (apply do-part 18))
(and->> (str-thunks 19 day19:part-1 day19:part-2) (apply do-part 19))

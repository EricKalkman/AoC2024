(library (day10)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (grids))

  (define (get-all-peaks g)
    (define trailheads
      (->> (all-coords-of char=? g #\0)
           (map list)))
    (->> '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (fold-left
           (λ (trails elevation)
              (map (λ (coords)
                      (->> (mapcat neighbors4 coords)
                           (filter (λ (c) (and (in-grid? g c)
                                               (char=? elevation (grid-get g c)))))))
                   trails))
           trailheads)))

  (define (part-1 s)
    (->> s
         (string->grid)
         (get-all-peaks)
         (map (compose vector-length dedup))
         (apply +)))

  (define (part-2 s)
    (->> s
         (string->grid)
         (get-all-peaks)
         (map length)
         (apply +)))
)

#|
(import (rnrs) (util) (grids) (day10))


(define test-inp (file->string "../inputs/day10.test"))
(define real-inp (file->string "../inputs/day10.inp"))
;(grid-show test-inp)

(part-1 test-inp) ; 36
(part-1 real-inp) ; 646

(part-2 test-inp) ; 81
(part-2 real-inp) ; 1494
|#

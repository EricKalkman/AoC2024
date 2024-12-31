(library (day04)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (grids))

  (define (part-1 s)
    (define g (string->grid s))
    (define spellers (map (λ (dir) (λ (start) (grid-spells-along? g "XMAS" dir start)))
                          DIRS8))
    (->> (all-coords-of char=? g #\X)
         (map (λ (c) (count (λ (s) (s c)) spellers)))
         (apply +)))

  (define (part-2 s)
    (define g (string->grid s))
    (define spellers
      (->> '(NE SE SW NW)
           (map (λ (dir)
                   (λ (start)
                      (grid-spells-along? g "MAS" dir
                                          (point-move dir -1 start)))))))
    (->> (all-coords-of char=? g #\A)
         (count (λ (c) (= 2 (count (λ (s) (s c)) spellers))))))
)

#|
(import (rnrs) (util) (day04) (grids))

(define test-inp (file->grid "../inputs/day04.test"))
(define real-inp (file->grid "../inputs/day04.inp"))

(part-1 test-inp) ; 18
(part-1 real-inp) ; 2297

(part-2 test-inp) ; 9
(part-2 real-inp) ; 1745
|#

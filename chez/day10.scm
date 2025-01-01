(library (day10)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (grids))

  (define (part-1 s)
    (define g (string->grid s))
    (define trailheads
      (->> (all-coords-of char=? g #\0)
           (map (λ (c)
                   (let ([res (make-hashtable equal-hash equal? 16)])
                     (mut-> res (hashtable-set! c #t)))))))
    (->> '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (fold-left
           (λ (trails elevation)
              (->> trails
                   (map
                     (λ (set)
                        (vector-fold-left
                          (λ (new-set cs)
                             (fold-left (λ (new-set c) (mut-> new-set (hashtable-set! c #t)))
                                        new-set
                                        cs))
                          (make-hashtable equal-hash equal? 16)
                          (vector-map 
                            (λ (c) (->> (neighbors4 c)
                                        (filter (λ (n) (and (in-grid? g n)
                                                            (char=? elevation (grid-get g n)))))))
                            (hashtable-keys set)))))))
           trailheads)
         (map hashtable-size)
         (apply +)))

  (define (part-2 s)
    (define g (string->grid s))
    (define trailheads
      (->> (all-coords-of char=? g #\0)
           (map list)))
    (->> '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (fold-left
           (λ (trails elevation)
              (->> trails
                   (map (λ (coords)
                           (->> (mapcat neighbors4 coords)
                                (filter (λ (c) (and (in-grid? g c)
                                                    (char=? elevation (grid-get g c))))))))))
           trailheads)
         (map length)
         (apply +)))

)

(import (rnrs) (util) (grids) (day10))

#|
(define test-inp (file->grid "../inputs/day10.test"))
(define real-inp (file->grid "../inputs/day10.inp"))
;(grid-show test-inp)

(part-1 test-inp) ; 36
(part-1 real-inp) ; 646

(part-2 test-inp) ; 81
(part-2 real-inp) ; 1494
|#

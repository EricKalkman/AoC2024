(library (day06)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (grids))

  (define (neighbor grid node)
    (define coord (car node))
    (define dir (cdr node))
    (define next-coord (point-move dir 1 coord))
    (cond
      [(not (in-grid? grid next-coord)) #f]
      [(char=? #\# (grid-get grid next-coord)) (cons coord (turn-right dir))]
      [else (cons next-coord dir)]))

  (define (nodes-visited grid start)
    (let loop ([node (cons start 'up)]
               [visited? (mut-> (make-hashtable equal-hash equal?) (hashtable-set! start #t))])
      (if-let ([neigh (neighbor grid node)])
        (loop neigh (mut-> visited? (hashtable-update! (car neigh) identity (cdr neigh))))
        visited?)))

  (define (part-1 s)
    (define grid (string->grid s))
    (define start (coords-of char=? grid #\^))
    (->> (nodes-visited grid start)
         (hashtable-size)))

  (define (next-turn grid node)
    (define coord (car node))
    (define dir (cdr node))
    (let loop ([coord coord])
      (cond
        [(not (in-grid? grid coord)) #f]
        [(char=? #\# (grid-get grid coord)) (cons (point-move dir -1 coord) (turn-right dir))]
        [else (loop (point-move dir 1 coord))])))

  (define (in-cycle? grid start)
    (let loop ([node start]
               [visited? (mut-> (make-hashtable equal-hash equal?)
                                (hashtable-set! start #t))])
      (if-let ([neigh (next-turn grid node)])
        (if (hashtable-ref visited? neigh #f)
          neigh
          (loop neigh (mut-> visited? (hashtable-set! neigh #t))))
        #f)))

  (define (part-2 s)
    (define grid (string->grid s))
    (define start (coords-of char=? grid #\^))
    (define visited? (nodes-visited grid start))
    (hashtable-delete! visited? start)
    (->> visited?
         (hashtable-keys)
         (vector-count
           (λ (obst-pos)
              (grid-set! grid obst-pos #\#)
              (let* ([dir (hashtable-ref visited? obst-pos #f)]
                     [start (point-move dir -1 obst-pos)]
                     [res (in-cycle? grid (cons start dir))])
                (grid-set! grid obst-pos #\.)
                res)))))
)

#|
(import (rnrs) (day06) (grids) (util))

(define test-inp "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(define g (string->grid test-inp))
(define start (coords-of char=? g #\^)) ; (6 . 4)
(part-1 test-inp) ; 41
(display '---) (newline)
(part-1 (file->string "../inputs/day06.inp")) ; 4696
(part-2 test-inp) ; 6
(part-2 (file->string "../inputs/day06.inp")) ; 1443
|#

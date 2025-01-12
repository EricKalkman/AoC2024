(library (day16)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (grids)
                 (prefix (searches) s:))

  (define MOVE-COST 1)
  (define TURN-COST 1000)

  (define (neighbors grid node)
    (define coord (car node))
    (define dir (cdr node))
    (->> `(((,(point-move dir 1 coord) . ,dir) . ,MOVE-COST)
           ((,(point-move (turn-left dir) 1 coord) . ,(turn-left dir)) . ,(+ MOVE-COST TURN-COST))
           ((,(point-move (turn-right dir) 1 coord) . ,(turn-right dir)) . ,(+ MOVE-COST TURN-COST)))
         (filter (λ (c)
                    (and (in-grid? grid (caar c))
                         (char=? #\. (grid-get grid (caar c))))))))

  (define (all-tiles-on-shortest-path start end res)
    (define prevs (assv-get 'prevs res))
    (define lasts (assv-get 'lasts res))
    (assert (null? (cdr lasts)))
    (->> (s:bfs (λ (n) (cdr (hashtable-ref prevs n #f)))
                (car lasts)
                (λ (n) (equal? (car n) start)))
         (assv-get 'prevs)
         (hashtable-keys)
         (vector-fold-left
           (λ (ht n) (mut-> ht (hset-incl! (car n))))
           (make-hashtable equal-hash equal? 32))
         (hashtable-size)))

  (define (part finalizer s)
    (define g (string->grid s))
    (define start (coords-of char=? g #\S))
    (define end (coords-of char=? g #\E))
    (grid-set! g start #\.)
    (grid-set! g end #\.)
    (->> (s:dijkstra
           (λ (n) (neighbors g n))
           (cons start 'right)
           (λ (n) (equal? (car n) end)))
         (finalizer start end)))

  (define part-1 (partial part (λ (_start _end res) (assv-get 'cost res))))
  (define part-2 (partial part all-tiles-on-shortest-path))
)
#|
(import (rnrs) (util) (day16))

(define test-inp (file->string "../inputs/day16.test"))
(define real-inp (file->string "../inputs/day16.inp"))

(part-1 test-inp) ; 7036
(part-1 real-inp) ; 95476
(part-2 test-inp) ; 45
(part-2 real-inp) ; 511
|#

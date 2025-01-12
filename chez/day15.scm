(library (day15)
         (export line pgrid pinput part-1 char->dir part-2)
         (import (rnrs)
                 (util)
                 (prefix (parsecomb) p:)
                 (grids)
                 (prefix (searches) s:)
                 (transducers))

  (define line (p:charp+ (λ (c) (not (char=? #\newline c)))))
  (define pgrid (->> (p:list-of line p:nl)
                     (p:pmap
                       (λ (lines)
                          (let* ([g (grid-from-lines lines)]
                                 [start (coords-of char=? g #\@)]
                                 [walls (all-coords-of char=? g #\#)]
                                 [stones (all-coords-of char=? g #\O)])
                            (list start
                                  (list->hashset stones)
                                  (list->hashset walls)))))))
  (define (char->dir c)
    (case c
      [(#\<) 'left]
      [(#\>) 'right]
      [(#\v) 'down]
      [(#\^) 'up]))
  (define pcommands (->> (p:list-of line p:nl)
                         (p:pmap (λ (lst)
                                    (->> (apply string-append lst)
                                         (string->list)
                                         (map char->dir))))))
  (define pinput (p:seq pgrid p:skip-nl p:skip-nl
                        pcommands))

  (define (num-to-push stones walls dir start)
    (let loop ([count 0]
               [coord (point-move dir 1 start)])
      (cond
        [(hset-contains? walls coord) #f]
        [(hset-contains? stones coord) (loop (+ count 1) (point-move dir 1 coord))]
        [else count])))

  (define (stones-push! stones dir start n)
    (define end-coord (point-move dir (+ n 1) start))
    (hashtable-delete! stones (point-move dir 1 start))
    (hset-incl! stones end-coord))

  (define (gps-checksum stones)
    (->> (hashtable-keys stones)
         (vector-fold-left
           (λ (acc p) (+ acc (* 100 (point-row p)) (point-col p)))
           0)))

  (define (part-1 s)
    (define inp (p:parse-result-unwrap (p:parse pinput s)))
    (define start (caar inp))
    (define stones (cadar inp))
    (define walls (caddar inp))
    (define cmds (cadr inp))
    (let loop ([coord start]
               [cmds cmds])
      (unless (null? cmds)
        (let* ([dir (car cmds)]
               [n (num-to-push stones walls dir coord)])
          (case n
            ; ran into wall; can't push
            [(#f) (loop coord (cdr cmds))]
            ; no stones to push
            [(0) (loop (point-move dir 1 coord) (cdr cmds))]
            [else
              (stones-push! stones dir coord n)
              (loop (point-move dir 1 coord) (cdr cmds))]))))
    (gps-checksum stones))

  (define (make-doublewide stones walls)
    (values
      (->> (hashtable-keys stones)
           (vector-fold-left
             (λ (ht x)
                (let ([new-pt (make-point (point-row x) (* 2 (point-col x)))])
                  (mut-> ht (hset-incl! new-pt))))
             (make-hashtable equal-hash equal? 32)))
      (->> (hashtable-keys walls)
           (vector-fold-left
             (λ (ht x)
                (let* ([new-pt (make-point (point-row x) (* 2 (point-col x)))]
                       [right (point-move 'right 1 new-pt)])
                  (mut-> ht
                         (hset-incl! new-pt)
                         (hset-incl! right))))
             (make-hashtable equal-hash equal? 32)))))

  (define (stone-at stones coord)
    (or (hset-contains? stones coord)
        (hset-contains? stones (point-move 'left 1 coord))))

  (define (horizontal? dir) (memv dir '(left right)))
  (define (vertical? dir) (memv dir '(up down)))

  (define (neighbors stones walls dir from)
    (define step (point-move dir 1 from))
    (if-let ([stone-root (stone-at stones step)])
      (list stone-root (point-move 'right 1 stone-root))
      (if (hset-contains? walls step) (list step) '())))

  (define (stones-move! stones dir prevs)
    (let ([moved-stones (->> (hashtable-keys prevs)
                             (transduce-vector
                               (tfilter (λ (p) (hset-contains? stones p)))
                               rcons '()))])
      (fold-left
        (λ (stones p) (mut-> stones (hashtable-delete! p)))
        stones moved-stones)
      (fold-left
        (λ (stones p)
           (let ([next-coord (point-move dir 1 p)])
             (mut-> stones (hset-incl! next-coord))))
        stones moved-stones)))

  (define (part-2 s)
    (define inp (p:parse-result-unwrap (p:parse pinput s)))
    (define start (caar inp))
    (define shifted-start (make-point (point-row start) (* 2 (point-col start))))
    (define cmds (cadr inp))
    (let-values ([(stones walls) (make-doublewide (cadar inp) (caddar inp))])
      (let loop ([cmds cmds]
                 [coord shifted-start])
        (unless (null? cmds)
          (let* ([dir (car cmds)]
                 [bfs-result (s:bfs (λ (p) (neighbors stones walls dir p))
                                    coord
                                    (λ (p) (hset-contains? walls p)))])
            (if (assv 'last bfs-result) ; ended at a wall
              (loop (cdr cmds) coord)
              (begin
                (->> (assv-get 'prevs bfs-result)
                     (stones-move! stones dir))
                (loop (cdr cmds) (point-move dir 1 coord)))))))
      (gps-checksum stones)))
)
#|
(import (rnrs) (util) (day15) (parsecomb) (grids))
(define test-inp (file->string "../inputs/day15.test1"))
(define test-inp2 (file->string "../inputs/day15.test"))
(define test-inp3 (file->string "../inputs/day15.test2"))
(define real-inp (file->string "../inputs/day15.inp"))

;(part-1 test-inp) ; 2028
;(part-1 test-inp2) ; 10092
;(part-1 real-inp) ; 1412971

(part-2 test-inp3) ; 618
(part-2 test-inp2) ; 9021
(part-2 real-inp) ; 1429299
|#

(library (day08)
         (export part-1 part-2 find-antennae group-antennae coords-in-line)
         (import (rnrs)
                 (util)
                 (prefix (grids) g:))

(define (find-antennae g)
  (define h (g:grid-height g))
  (define w (g:grid-width g))
  (let loop ([row 0]
             [col 0]
             [res '()])
    (cond
      [(>= row h) res]
      [(>= col w) (loop (+ row 1) 0 res)]
      [(not (char=? #\. (g:grid-get g row col)))
       (loop row (+ 1 col) (cons (cons (g:make-point row col) (g:grid-get g row col))
                                 res))]
      [else (loop row (+ 1 col) res)])))

  (define (group-antennae ants)
    (define res (make-eqv-hashtable))
    (->> ants
         (for-each (λ (c) (hashtable-update! res (cdr c)
                                             (λ (lst) (cons (car c) lst))
                                             '()))))
    res)

  (define (reduction-factor delta)
    (if (or (zero? (g:point-row delta)) (zero? (g:point-col delta)))
      (argmax abs (g:point-row delta) (g:point-col delta))
      (gcd (abs (g:point-row delta)) (abs (g:point-col delta)))))

  (define (coords-in-line g a b)
    (define delta (g:point- b a))
    (define denom (reduction-factor delta))
    (define scaled-delta (g:make-point (/ (g:point-row delta) denom) (/ (g:point-col delta) denom)))
    (let loop ([coord a]
               [res '()])
      (if (g:in-grid? g coord)
        (loop (g:point+ coord scaled-delta) (cons coord res))
        (reverse res))))

  (define (first-double-dist a b coords)
    (define delta (g:point- b a))
    (define denom (reduction-factor delta))
    (define idx (* 2 denom))
    (and (< idx (length coords)) (list-ref coords idx)))

  (define (combinations lst)
    (let loop ([lst lst]
               [acc '()])
      (if (null? (cdr lst))
        acc
        (loop (cdr lst)
              (append (map (λ (x) (cons (car lst) x)) (cdr lst))
                      acc)))))

  (define (single-antinodes-of g a b)
    (->> (list (list a b) (list b a))
         (map (λ (p) (first-double-dist (car p) (cadr p)
                                        (coords-in-line g (car p) (cadr p)))))
         (filter identity)))

  (define (all-antinodes antinoder g as)
    ; note: there are duplicate coordinates here. ignored because they are combined
    ; into a set later
    (->> (combinations as)
         (map (λ (p) (antinoder g (car p) (cdr p))))
         (apply append)))

  (define (part antinoder s)
    (define g (g:string->grid s))
    (define ants (->> g (find-antennae) (group-antennae)))
    (define res (make-hashtable equal-hash equal?))
    (let-values ([(_ groups) (hashtable-entries ants)])
      (->> groups
           (vector-map (λ (coords) (all-antinodes antinoder g coords)))
           (vector-for-each
             (λ (cs)
                (for-each (λ (x) (hashtable-set! res x #t)) cs))))
      (hashtable-size res)))

  (define part-1 (partial part single-antinodes-of))

  (define (all-antinodes-of g a b)
    (append (coords-in-line g a b)
            (coords-in-line g b a)))

  (define part-2 (partial part all-antinodes-of))
)

#|
(import (rnrs) (grids) (util) (day08))

(define test-inp "..........
..........
..........
....a.....
..........
.....a....
..........
..........
..........
..........")

(define test-inp2 "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(define g1 (string->grid test-inp))
(define g2 (string->grid test-inp2))

(define freqs (->> g1 (find-antennae) (group-antennae)))
;(hashtable-values freqs)
(define as (hashtable-ref freqs #\a #f))
(define a (car as))
(define b (cadr as))
(coords-in-line g1 a b)

(part-1 test-inp) ; 2
(part-1 test-inp2) ; 14
(part-1 (file->string "../inputs/day08.inp")) ; 329

(part-2 test-inp) ; 5
(part-2 test-inp2) ; 34
(part-2 (file->grid "../inputs/day08.inp")) ; 1190
|#

(library (day12)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (grids)
                 (searches))

  (define (neighbors g coord)
    (define color (grid-get g coord))
    (->> (neighbors4 coord)
         (filter (λ (n)
                    (and (in-grid? g n)
                         (char=? color (grid-get g n)))))))

  (define (subset? p q)
    (for-all (λ (x) (memv x q)) p))

  (define (num-convex-corners dirs)
    (->> '((up right) (up left) (down right) (down left))
         (count (λ (set) (subset? set dirs)))))

  (define (num-concave-corners g dirs cur)
    (let ([color (grid-get g cur)]
          [inside-dirs (fold-left (λ (set x) (remv x set)) DIRS4 dirs)])
      ;(display inside-dirs) (newline)
      (->> '((up right) (right down) (down left) (left up))
           (count
             (λ (set)
                (and (subset? set inside-dirs)
                     (let ([n (->> cur
                                   (point-move (car set) 1)
                                   (point-move (cadr set) 1))])
                       (or (not (in-grid? g n))
                           (not (char=? color (grid-get g n)))))))))))

  (define (border-property-visit g perimeter n-corners)
    (case-lambda
      ([] `((perimeter . ,perimeter) (n-corners . ,n-corners)))
      ([_prevs cur]
       (let* ([color (grid-get g cur)]
              [ds (->> DIRS4
                      (filter (λ (dir)
                              (let ([next-coord (point-move dir 1 cur)])
                                (if (in-grid? g next-coord)
                                  (not (eqv? color (grid-get g next-coord)))
                                  #t)))))])
         (border-property-visit
           g
           (+ perimeter (length ds))
           (+ n-corners
              (num-convex-corners ds)
              (num-concave-corners g ds cur)
              ))))))

  (define (find-plots g)
    (define h (grid-height g))
    (define w (grid-width g))
    (define total-size (* h w))
    (let loop ([row 0]
               [col 0]
               [seen-coords (make-hashtable equal-hash equal?)]
               [plots '()])
      (cond
        [(or (>= row h) (= total-size (hashtable-size seen-coords)))
         plots]
        [(>= col w) (loop (+ row 1) 0 seen-coords plots)]
        [(hashtable-ref seen-coords (make-point row col) #f)
         (loop row (+ 1 col) seen-coords plots)]
        [else
          (let* ([cur (make-point row col)]
                 [b (bfs (partial neighbors g)
                         (make-point row col)
                         (λ _ #f)
                         (border-property-visit g 0 0))]
                 [prevs (assv-get 'prevs b)]
                 [visit ((assv-get 'visit b))])
            (loop row (+ 1 col)
                  (vector-fold-left
                    (λ (seen c) (mut-> seen (hashtable-set! c #t)))
                    seen-coords (hashtable-keys prevs))
                  (cons (list (cons 'type (grid-get g cur))
                              (cons 'count (hashtable-size prevs))
                              (cons 'perimeter (assv-get 'perimeter visit))
                              (cons 'n-corners (assv-get 'n-corners visit)))
                        plots)))])))

  (define (part-1 s)
    (->> s
         (string->grid)
         (find-plots)
         (map (λ (res) (* (assv-get 'perimeter res) (assv-get 'count res))))
         (apply +)))

  (define (part-2 s)
    (->> s
         (string->grid)
         (find-plots)
         (map (λ (res) (* (assv-get 'count res) (assv-get 'n-corners res))))
         (apply +)))
)

#|
(import (rnrs) (util) (grids) (day12))

(define test-grid (file->grid "../inputs/day12.test"))
(define real-grid (file->grid "../inputs/day12.inp"))

(part-1 test-grid) ; 1930
(part-1 real-grid) ; 1446042
(part-2 test-grid) ; 1206
(part-2 real-grid) ; 902742
|#

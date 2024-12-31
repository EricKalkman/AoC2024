(library (grids)
         (export
           grid make-grid grid?
           grid-height grid-width
           grid-show
           grid-from-lines
           file->grid
           string->grid

           in-grid?
           grid-get
           grid-set!
           coords-of all-coords-of

           DIRS4 DIRS8 point-move
           dir-flip turn-right turn-left
           neighbors4
           grid-neighbors4
           point+ point- point*
           dir->point

           trace-coords
           trace-coords-in-grid
           trace-grid
           grid-spells-along?
           )
         (import (rnrs) (util))

  (define-record-type
    (grid %make-grid grid?)
    (fields
      (mutable arr)
      (immutable height)
      (immutable width)))

  (define grid-show
    (case-lambda
      [(g) (grid-show g identity)]
      [(g charify)
       (define h (grid-height g))
       (define w (grid-width g))
       (let loop ([row 0]
                  [col 0])
         (cond
           [(= col w) (newline) (loop (+ row 1) 0)]
           [(< row h)
            (display (charify (grid-get g (cons row col))))
            (loop row (+ 1 col))]))]))

  (define make-grid
    (case-lambda
      ([height width] (make-grid height width #f))
      ([height width fill] (%make-grid (make-vector (* height width) fill)
                                       height width))))

  (define grid-from-lines
    (case-lambda
      ([lines] (grid-from-lines lines identity))
      ([lines mapper]
       (define height (length lines))
       (define width (fold-left (λ (acc x) (min acc (string-length x)))
                                (string-length (car lines))
                                (cdr lines)))
       (define g (make-grid height width))
       (define v (grid-arr g))
       (let loop ([row 0]
                  [col 0]
                  [lines lines])
         (cond
           [(= row height) g]
           [(= col width) (loop (+ row 1) 0 (cdr lines))]
           [else
             (vector-set! v (+ (* row width) col)
                          (mapper (string-ref (car lines) col)))
             (loop row (+ 1 col) lines)])))))

  (define file->grid
    (case-lambda
      ([fname] (file->grid fname identity))
      ([fname mapper]
       (call-with-input-file fname
         (λ (p)
            (let loop ([res (get-line p)]
                       [acc '()])
              (if (eof-object? res)
                (grid-from-lines (reverse acc) mapper)
                (loop (get-line p) (cons res acc)))))))))

  (define string->grid
    (case-lambda
      ([s] (string->grid s identity))
      ([s mapper]
       (call-with-port (open-string-input-port s)
         (λ (p)
            (let loop ([line (get-line p)]
                       [acc '()])
              (if (eof-object? line)
                (grid-from-lines (reverse acc) mapper)
                (loop (get-line p) (cons line acc)))))))))

  (define in-grid?
    (case-lambda
      ([g coord] (in-grid? g (car coord) (cdr coord)))
      ([g row col] (and (<= 0 row (- (grid-height g) 1))
                        (<= 0 col (-  (grid-width g) 1))))))

  (define grid-get
    (case-lambda
      ([g coord] (grid-get g (car coord) (cdr coord)))
      ([g row col] (vector-ref (grid-arr g)
                               (+ col (* row (grid-width g)))))))

  (define grid-set!
    (case-lambda
      ([g coord x] (grid-set! g (car coord) (cdr coord) x))
      ([g row col x] (vector-set! (grid-arr g)
                                  (+ col (* row (grid-width g)))
                                  x))))

  (define (coords-of egal? grid x)
    (define w (grid-width grid))
    (define h (grid-height grid))
    (let loop ([row 0]
               [col 0])
      (cond
        [(>= row h) #f]
        [(>= col w) (loop (+ row 1) 0)]
        [(egal? x (grid-get grid row col)) (cons row col)]
        [else (loop row (+ col 1))])))

  (define (all-coords-of egal? grid x)
    (define h (grid-height grid))
    (define w (grid-width grid))
    (let loop ([row 0]
               [col 0]
               [acc '()])
      (cond
        [(>= row h) acc]
        [(>= col w) (loop (+ row 1) 0 acc)]
        [(egal? x (grid-get grid row col))
         (loop row (+ col 1) (cons (cons row col) acc))]
        [else (loop row (+ col 1) acc)])))

  (define DIRS4 '(up right down left))
  (define DIRS8 '(up right down left NE SE SW NW))
  
  (define (dir-flip dir)
    (case dir
      [(up) 'down]
      [(right) 'left]
      [(down) 'up]
      [(left) 'right]
      [(NE) 'SW]
      [(SE) 'NW]
      [(SW) 'NE]
      [(NW) 'SE]))

  (define (turn-right dir)
    (case dir
      [(up) 'right]
      [(right) 'down]
      [(down) 'left]
      [(left) 'up]
      [(NE) 'SE]
      [(SE) 'SW]
      [(SW) 'NW]
      [(NW) 'NE]))

  (define (turn-left dir)
    (case dir
      [(up) 'left]
      [(right) 'up]
      [(down) 'right]
      [(left) 'down]
      [(NE) 'NW]
      [(SE) 'NE]
      [(SW) 'SE]
      [(NW) 'SW]))

  (define (dir->point dir)
    (case dir
      [(up) (cons -1 0)]
      [(right) (cons 0 1)]
      [(down) (cons 1 0)]
      [(left) (cons 0 -1)]
      [(NE) (cons -1 1)]
      [(SE) (cons 1 1)]
      [(SW) (cons 1 -1)]
      [(NW) (cons -1 -1)]))

  (define (point-move dir n coord)
    (let* ([d1 (dir->point dir)]
           [delta (point* n d1)])
      (point+ coord delta)))

  (define (neighbors4 coord)
    (map (λ (dir) (point-move dir 1 coord)) DIRS4))

  (define (grid-neighbors4 g coord)
    (->> (neighbors4 coord)
         (filter (λ (c) (in-grid? g c)))))

  (define (point+ a b)
    (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))
  (define (point- a b)
    (cons (- (car a) (car b)) (- (cdr a) (cdr b))))
  (define (point* k a)
    (cons (* k (car a)) (* k (cdr a))))

  (define (trace-coords dir start n)
    (if (<= n 0)
      '()
      (cons start
            (trace-coords dir (point-move dir 1 start) (- n 1)))))

  (define trace-coords-in-grid
    (case-lambda
      [(g dir start)
       (if (not (in-grid? g start))
         '()
         (cons start (trace-coords-in-grid g dir (point-move dir 1 start))))]
      [(g dir start n)
       (if (or (<= n 0) (not (in-grid? g start)))
         '()
         (cons start (trace-coords-in-grid g dir (point-move dir 1 start) (- n 1))))]))

  (define trace-grid
    (case-lambda
      [(g dir start)
       (if (not (in-grid? g start))
         '()
         (cons (grid-get g start)
               (trace-grid g dir (point-move dir 1 start))))]
      [(g dir start n)
       (if (or (<= n 0) (not (in-grid? g start)))
         '()
         (cons (grid-get g start)
               (trace-grid g dir (point-move dir 1 start) (- n 1))))]))

  (define (grid-spells-along? g s dir start)
    (let ([slist (string->list s)]
          [trace (trace-grid g dir start (string-length s))])
      (equal? slist trace)))
)

#|
(import ( grids))

(point-move 'right 1 '(0 . 0))
|#

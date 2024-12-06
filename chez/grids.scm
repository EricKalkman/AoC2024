(library (grids)
         (export
           grid make-grid grid?
           grid-height grid-width
           grid-from-lines
           file->grid
           string->grid

           in-grid?
           grid-get
           grid-set!
           coords-of

           DIRS4 point-move
           dir-flip turn-right turn-left
           neighbors4
           grid-neighbors4
           )
         (import (rnrs) (util))

  (define-record-type
    (grid %make-grid grid?)
    (fields
      (mutable arr)
      (immutable height)
      (immutable width)))

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


  (define DIRS4 '(up right down left))
  
  (define (dir-flip dir)
    (case dir
      [(up) 'down]
      [(right) 'left]
      [(down) 'up]
      [(left) 'right]))

  (define (turn-right dir)
    (case dir
      [(up) 'right]
      [(right) 'down]
      [(down) 'left]
      [(left) 'up]))

  (define (turn-left dir)
    (case dir
      [(up) 'left]
      [(right) 'up]
      [(down) 'right]
      [(left) 'down]))

  (define (point-move dir n coord)
    (case dir
      [(up) (cons (- (car coord) n) (cdr coord))]
      [(down) (cons (+ (car coord) n) (cdr coord))]
      [(left) (cons (car coord) (- (cdr coord) n))]
      [(right) (cons (car coord) (+ (cdr coord) n))]))

  (define (neighbors4 coord)
    (map (λ (dir) (point-move dir 1 coord)) DIRS4))

  (define (grid-neighbors4 g coord)
    (->> (neighbors4 coord)
         (filter (λ (c) (in-grid? g c)))))
)

#|
(import ( grids))

(point-move 'right 1 '(0 . 0))
|#

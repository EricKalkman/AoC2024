(library (day09)
         (export parse-input part-1 part-2)
         (import (rnrs)
                 (util)
                 (binomial-heaps)
                 (prefix (parsecomb) p))

  (define chunk-pos car)
  (define chunk-len cadr)
  (define chunk-id caddr)

  (define (parse-input s)
    (define new-s (string-append (string-trim-right s) "0"))
    (let loop ([idx 0]
               [pos 0]
               [file-id 0]
               [files '()]
               [spaces '()])
      (if (>= idx (string-length new-s))
        (cons files (filter (λ (x) (not (zero? (chunk-len x)))) spaces))
        (let ([len (- (char->integer (string-ref new-s idx)) (char->integer #\0))])
          (if (even? idx)
            (loop (+ idx 1) (+ pos len) (+ file-id 1)
                  (cons (list pos len file-id) files) spaces)
            (loop (+ idx 1) (+ pos len) file-id
                  files (cons (list pos len) spaces)))))))

  (define (move-files-blockwise rev-files spaces)
    (cond
      [(null? rev-files) (error 'move-files-blockwise "there should always be a file")]
      [(zero? (chunk-len (car rev-files))) (move-files-blockwise (cdr rev-files) spaces)]
      [(or (null? spaces) (< (chunk-pos (car rev-files)) (chunk-pos (car spaces))))
       rev-files]
      [(zero? (chunk-len (car spaces))) (move-files-blockwise rev-files (cdr spaces))]
      [else
        (let* ([space (car spaces)]
               [spos (chunk-pos space)]
               [slen (chunk-len space)]
               [file (car rev-files)]
               [fid (chunk-id file)]
               [fpos (chunk-pos file)]
               [flen (chunk-len file)]
               [to-consume (min slen flen)])
          (cons (list spos to-consume fid)
                (move-files-blockwise
                  (cons (list fpos (- flen to-consume) fid) (cdr rev-files))
                  (cons (list (+ spos to-consume) (- slen to-consume)) (cdr spaces)))))]))

  (define (triangle-sum first last)
    (/ (* (+ 1 (- last first)) (+ first last)) 2))

  (define (checksum files)
    (->> files
         (map (λ (file) (* (chunk-id file)
                          (triangle-sum (chunk-pos file)
                                        (+ (chunk-pos file) (chunk-len file) -1)))))
         (apply +)))

  (define (move-files-leftward files spaces)
    (define (aux files spaces)
      (if (null? files)
        files
        (let* ([file (car files)]
               [files (cdr files)]
               [leftmost-space
                 (->> '(1 2 3 4 5 6 7 8 9)
                      (filter-map
                        (λ (len)
                           (if (or (< len (chunk-len file))
                                   (bin-heap-empty? (vector-ref spaces len)))
                             #f
                             (vector-ref spaces len))))
                      (fold-left
                        (λ (m set)
                           (let ([x (min-elt set)])
                             (if (< (chunk-pos x) (chunk-pos m))
                               x m)))
                        (list +inf.0 0)))])
          (if (< (chunk-pos file) (chunk-pos leftmost-space))
            (cons file (aux files spaces))
            (begin
              (let-values ([(_ set) (pop-min (vector-ref spaces (chunk-len leftmost-space)))])
                (vector-set! spaces (chunk-len leftmost-space) set)
                (when (> (chunk-len leftmost-space) (chunk-len file))
                  (let* ([new-len (- (chunk-len leftmost-space) (chunk-len file))]
                         [new-space (list (+ (chunk-pos leftmost-space) (chunk-len file))
                                          (- (chunk-len leftmost-space) (chunk-len file)))])
                    (vector-set! spaces new-len (push (vector-ref spaces new-len) new-space))))
                (cons (list (chunk-pos leftmost-space) (chunk-len file) (chunk-id file))
                      (aux files spaces))))))))

    (let loop ([spaces-arr (make-vector 10 (make-empty-bin-heap-by chunk-pos))]
               [spaces spaces])
      (if (null? spaces)
        (aux files spaces-arr)
        (let ([space (car spaces)]
              [spaces (cdr spaces)])
          (vector-set! spaces-arr (chunk-len space)
                       (push (vector-ref spaces-arr (chunk-len space))
                             space))
          (loop spaces-arr spaces)))))

  (define (part mover s)
    (define inp (parse-input s))
    (define rev-files (car inp))
    (define rev-spaces (cdr inp))
    (->> (mover rev-files (reverse rev-spaces))
         (checksum)))

  (define part-1 (partial part move-files-blockwise))
  (define part-2 (partial part move-files-leftward))
)

#|
(import (rnrs) (util) (parsecomb) (day09) (binomial-heap))

(define test-inp (file->string "../inputs/day09.test"))
(define real-inp (file->string "../inputs/day09.inp"))

(part-1 test-inp) ; 1928
(part-1 real-inp) ; 6310675819476

(part-2 test-inp) ; 2858
(part-2 real-inp) ; 6335972980679
|#

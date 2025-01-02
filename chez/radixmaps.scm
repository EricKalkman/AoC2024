(library (radmaps)
         (export radmap %make-radmap radmap?
                 make-radmap
                 radmap-ref
                 radmap-set! radmap-update! radmap-clear!
                 radmap-recount!
                 index->key
                 key->index
                 radmap-keys radmap-vals radmap-entries)
         (import (rnrs)
                 (util))
 
  ;; radix maps (speciallized hashmap using an array internally to store
  ;; multi-component keys such as points)
  (define-record-type
    (radmap %make-radmap radmap?)
    (fields
      (immutable radices)
      (immutable rev-radices)
      (mutable count)
      (immutable arr)))

  (define (make-radmap radices)
    (define max-len (fold-left * 1 radices))
    (%make-radmap (cdr radices) (reverse (cdr radices)) 0 (make-vector max-len #f)))

  (define (key->index rm k)
    (fold-left (λ (acc kdx radix) (+ (* acc radix) kdx))
               (car k)
               (cdr k) (radmap-radices rm)))

  (define (radmap-ref rm k)
    (vector-ref (radmap-arr rm) (key->index rm k)))

  (define (radmap-set! rm k v)
    (define idx (key->index rm k))
    (define vec (radmap-arr rm))
    (when (not (vector-ref vec idx))
      (radmap-count-set! rm (+ (radmap-count rm) 1)))
    (vector-set! vec idx v))

  (define (radmap-update! rm k f)
    (define idx (key->index rm k))
    (define vec (radmap-arr rm))
    (when (not (vector-ref vec idx))
      (radmap-count-set! rm (+ 1 (radmap-count rm))))
    (vector-set! vec idx (f (vector-ref vec idx))))

  (define (radmap-recount! rm)
    (define vec (radmap-arr rm))
    (let loop ([sum 0]
               [idx (- (vector-length vec) 1)])
      (cond
        [(< idx 0) sum]
        [(vector-ref vec idx) (loop (+ sum 1) (- idx 1))]
        [else (loop sum (- idx 1))])))

  (define (radmap-clear! rm)
    (vector-fill! (radmap-arr rm) #f)
    (radmap-count-set! rm 0))

  (define (index->key rm idx)
    (let loop ([acc '()]
               [idx idx]
               [radices (radmap-rev-radices rm)])
      (if (null? radices)
        (cons idx acc)
        (let-values ([(d r) (div-and-mod idx (car radices))])
          (loop (cons r acc)
                d
                (cdr radices))))))

  (define (radmap-keys rm)
    (define vec (radmap-arr rm))
    (let loop ([acc '()]
               [idx (- (vector-length vec) 1)])
      (cond
        [(< idx 0) acc]
        [(vector-ref vec idx)
         (loop (cons (index->key rm idx) acc)
               (- idx 1))]
        [else (loop acc (- idx 1))])))

  (define (radmap-vals rm)
    (define vec (radmap-arr rm))
    (let loop ([acc '()]
               [idx (- (vector-length vec) 1)])
      (cond
        [(< idx 0) acc]
        [(vector-ref vec idx)
         (loop (cons (vector-ref vec idx) acc)
               (- idx 1))]
        [else (loop acc (- idx 1))])))

  (define (radmap-entries rm)
    (define vec (radmap-arr rm))
    (let loop ([acc '()]
               [idx (- (vector-length vec) 1)])
      (cond
        [(< idx 0) acc]
        [(vector-ref vec idx)
         (loop (cons (cons (index->key rm idx)
                           (vector-ref vec idx))
                     acc)
               (- idx 1))]
        [else (loop acc (- idx 1))])))
)

#|
(import (rnrs) (util) (radmaps))

(define rm (make-radmap '(10 10 4)))

(radmap-set! rm '(1 1 3) 42)
(radmap-set! rm '(7 2 0) 73)

(radmap-entries rm)
|#

(library (deques)
         (export deque deque? make-deque
                 dq-length dq-empty?
                 list->deque deque->list
                 dq-enq! dq-enq-front!
                 dq-deq! dq-deq-back!
                 dq-peek dq-peek-back)
         (import (rnrs))

  (define-record-type (deque %deque deque?)
    (fields
      (mutable buf dq-buf set-dq-buf!)
      (mutable idx dq-idx set-dq-idx!)
      (mutable len dq-len set-dq-len!)))

  (define make-deque
    (case-lambda
      [() (make-deque 32)]
      [(count) (%deque (make-vector count) 0 0)]))

  (define (dq-cap q)
    (vector-length (dq-buf q)))

  (define dq-length dq-len)

  (define (dq-empty? q) (zero? (dq-len q)))

  (define (resize-dq-buf! dq new-len)
    (define buf (dq-buf dq))
    (define cap (vector-length buf))
    (define idx (dq-idx dq))
    (define len (dq-len dq))
    (define result (make-vector new-len))
    (when (> len new-len)
      (error 'resize-q-buf "Tried to shrink internal buf"))
    (let loop ([i 0])
      (when (< i len)
        (vector-set! result i (vector-ref buf (mod (+ i idx) cap)))
        (loop (+ i 1))))
    (set-dq-buf! dq result)
    (set-dq-idx! dq 0)
    )

  (define (dq-ref q idx)
    (vector-ref (dq-buf q) (mod (+ idx (dq-idx q)) (dq-cap q))))

  (define (dq-set! q idx x)
    (vector-set! (dq-buf q) (mod (+ idx (dq-idx q)) (dq-cap q)) x))

  (define (dq-enq! q x)
    (when (= (dq-len q) (vector-length (dq-buf q)))
      (resize-dq-buf! q (* 2 (dq-cap q))))
    (dq-set! q (dq-len q) x)
    (set-dq-len! q (+ 1 (dq-len q))))

  (define (dq-enq-front! q x)
    (when (= (dq-len q) (vector-length (dq-buf q)))
      (resize-dq-buf! q (* 2 (dq-cap q))))
    (set-dq-idx! q (mod (- (dq-idx q) 1) (dq-cap q)))
    (set-dq-len! q (+ 1 (dq-len q)))
    (dq-set! q 0 x))


  (define (dq-deq! q)
    (when (zero? (dq-len q))
      (error 'dq-deq! "tried to dequeue from empty queue"))
    (let ([res (dq-ref q 0)])
      (set-dq-idx! q (mod (+ 1 (dq-idx q)) (dq-cap q)))
      (set-dq-len! q (- (dq-len q) 1))
      res))

  (define (dq-deq-back! q)
    (when (zero? (dq-len q))
      (error 'dq-deq-back "tried to dequeue from empty queue"))
    (let ([res (dq-ref q (- (dq-len q) 1))])
      (set-dq-len! q (- (dq-len q) 1))
      res))

  (define (dq-peek q)
    (when (zero? (dq-len q))
      (error 'dq-peek "tried to peek empty queue"))
    (dq-ref q 0))

  (define (dq-peek-back q)
    (when (zero? (dq-len q))
      (error 'dq-peek-back "tried to peek empty queue"))
    (dq-ref q (- (dq-len q) 1)))

  (define (list->deque lst)
    (define len (length lst))
    (define q (make-deque (* 2 len)))
    (set-dq-len! q len)
    (let loop ([lst lst]
               [idx 0])
      (unless (null? lst)
        (vector-set! (dq-buf q) idx (car lst))
        (loop (cdr lst) (+ idx 1))))
    q)

  (define (deque->list q)
    (let loop ([idx (- (dq-len q) 1)]
               [lst '()])
      (if (< idx 0)
        lst
        (loop (- idx 1)
              (cons (dq-ref q idx) lst)))))
)

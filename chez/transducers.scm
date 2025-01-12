(library (transducers)
         (export transduce-list
                 transduce-vector
                 transduce-hashtable
                 transduce-hashset
                 transduce-generator
                 tmap tfilter
                 rcons
                 ttake tdrop
                 ttake-while tdrop-while
                 tenumerate
                 reduce-transposed)
         (import (rnrs)
                 (util))

  (define-record-type
    (reduced-box reduced reduced?)
    (fields (immutable x)))

  (define (transduce-list xform red init lst)
    (define xred (xform red))
    (let loop ([lst lst]
               [acc init])
      (cond
        [(reduced? acc) (reduced-box-x acc)]
        [(null? lst) acc]
        [else (loop (cdr lst) (xred acc (car lst)))])))

  (define (transduce-vector xform red init v)
    (define xred (xform red))
    (define len (vector-length v))
    (let loop ([idx 0]
               [acc init])
      (cond
        [(reduced? acc) (reduced-box-x acc)]
        [(>= idx len) acc]
        [else (loop (+ idx 1) (xred acc (vector-ref v idx)))])))

  (define (transduce-generator xform red init g)
    (define xred (xform red))
    (let loop ([acc init])
      (if (reduced? acc)
        (reduced-box-x acc)
        (let ([res (g)])
          (if (eof-object? res)
            acc
            (loop (xred acc res)))))))

  (define (transduce-hashset xform red init hs)
    (define xred (xform red))
    (define keys (hashtable-keys hs))
    (define len (vector-length keys))
    (let loop ([idx 0]
               [acc init])
      (cond
        [(reduced? acc) (reduced-box-x acc)]
        [(>= idx len) acc]
        [else (loop (+ idx 1) (xred acc (vector-ref keys idx)))])))

  (define (transduce-hashtable xform red init ht)
    (define xred (xform red))
    (let-values ([(keys vals) (hashtable-entries ht)])
      (let loop ([idx 0]
                 [acc init])
        (cond
          [(reduced? acc) (reduced-box-x acc)]
          [(>= idx (vector-length keys)) acc]
          [else (loop (+ idx 1)
                      (xred acc (cons (vector-ref keys idx) (vector-ref vals idx))))]))))

  (define (tmap f)
    (λ (red)
       (λ (acc x)
          (red acc (f x)))))

  (define (tfilter p)
    (λ (red)
       (λ (acc x)
          (if (p x)
            (red acc x)
            acc))))

  (define (ttake n)
    (λ (red)
       (define left n)
       (λ (acc x)
          (if (<= left 0)
            (reduced acc)
            (begin
              (set! left (- left 1))
              (red acc x))))))

  (define (tdrop n)
    (λ (red)
       (define left n)
       (λ (acc x)
          (if (<= left 0)
            (red acc x)
            (begin
              (set! left (- left 1))
              acc)))))

  (define (ttake-while p)
    (λ (red)
       (λ (acc x)
          (if (p x)
            (red acc x)
            (reduced acc)))))

  (define (tdrop-while p)
    (λ (red)
      (define dropped? #f)
       (λ (acc x)
          (cond
            [dropped? (red acc x)]
            [(p x) acc]
            [else (set! dropped? #t) (red acc x)]))))

  (define (tenumerate red)
    (define n 0)
    (λ (acc x)
       (define i n)
       (set! n (+ 1 n))
       (red acc (cons i x))))

  (define (reduce-transposed . fns)
    (λ (accs xs)
       (map (λ (fn acc x) (fn acc x)) fns accs xs)))
)
#|
(import (rnrs) (util) (transducers))

(transduce-list
  (ttake 3)
  rcons
  '()
  '(1 2 3 4))
|#

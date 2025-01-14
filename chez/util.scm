#!r6rs

(library (util)
         (export λ -> ->> as-> and-> and->> mut-> if-let
                 partial
                 string-contains? string-contains-sub?
                 identity
                 compose
                 argmin argmax
                 time-exec
                 file->lines
                 file->string
                 string->lines
                 acons
                 map* filter-map mapcat map-indexed
                 count
                 assv-get updatev
                 vector-count
                 swap-args
                 rcons
                 vector-fold-left
                 unfold
                 string-trim-right
                 dedup
                 vector-update!
                 take-while
                 list->hashset
                 hset-contains?
                 hset-incl!
                 split-at
                 groups-of
                 )
         (import (rnrs)
                 (rnrs r5rs)
                 (only (chezscheme) current-time time-second time-nanosecond time-difference)
                 )

  (define-syntax λ
    (syntax-rules ()
      [(_ args xs ...) (lambda args xs ...)]))

  (define-syntax ->
    (syntax-rules ()
      [(_ x) x]
      [(_ x (fn args ...) fns ...)
       (-> (fn x args ...)
           fns ...)]))

  (define-syntax ->>
    (syntax-rules ()
      [(_ x) x]
      [(_ x (fn args ...) fns ...)
       (->> (fn args ... x)
            fns ...)]))

  (define-syntax as->
    (syntax-rules ()
      [(_ x _) x]
      [(_ x name fn fns ...)
       (let ([name x])
         (as-> fn name fns ...))]))

  (define-syntax and->
    (syntax-rules ()
      [(_ x) x]
      [(_ x (fn args ...) fns ...)
       (let ([x* x])
         (if x*
           (and-> (fn x* args ...) fns ...)
           x*))]))

  (define-syntax and->>
    (syntax-rules ()
      [(_ x) x]
      [(_ x (fn args ...) fns ...)
       (let ([x* x])
         (if x*
           (and->> (fn args ... x) fns ...)
           x*))]))

  (define-syntax mut->
    (syntax-rules ()
      [(_ x) x]
      [(_ x (fn args ...) fns ...)
       (begin
         (let ([x* x])
           (fn x* args ...)
           (mut-> x* fns ...)))]))

  (define-syntax if-let
    (syntax-rules ()
      [(_ ()  t _) t]
      [(_ ((name val) bindings ...) t f)
       (let ([name val])
         (if name
           (if-let (bindings ...) t f)
           f))]))

  (define-syntax partial
    (syntax-rules ()
      [(_ fn args ...) (λ other-args (apply fn args ... other-args))]))

  (define (string-contains? s c)
    (define len (string-length s))
    (let loop ([idx 0])
      (cond
        [(>= idx len) #f]
        [(char=? c (string-ref s idx)) idx]
        [else (loop (+ 1 idx))])))

  (define (string-contains-sub? s sub)
    (define len (string-length s))
    (define sub-len (string-length sub))
    (let loop ([idx 0]
               [n-matched 0])
      (cond
        [(= idx len) #f]
        [(= n-matched sub-len) idx]
        [(= len (+ idx n-matched)) (loop (+ 1 idx) 0)]
        [(char=? (string-ref s (+ idx n-matched)) (string-ref sub n-matched))
         (loop idx (+ n-matched 1))]
        [else (loop (+ idx 1) 0)])))

  (define (identity x) x)

  (define compose
    (case-lambda
      ([f g] (λ args (f (apply g args))))
      ([f g h . fns]
       (define allfns (reverse (append (list f g h) fns)))
       (λ args
          (let loop ([fns (cdr allfns)]
                     [res (apply (car allfns) args)])
            (if (null? fns)
              res
              (loop (cdr fns) ((car fns) res))))))))

  (define (argmin keyfn . args)
    (let loop ([m (car args)]
               [km (keyfn (car args))]
               [xs args])
      (if (null? xs)
        m
        (let* ([x (car xs)]
               [kx (keyfn x)])
          (if (< kx km)
            (loop x kx (cdr xs))
            (loop m km (cdr xs)))))))

  (define (argmax keyfn . args)
    (let loop ([m (car args)]
               [km (keyfn (car args))]
               [xs args])
      (if (null? xs)
        m
        (let* ([x (car xs)]
               [kx (keyfn x)])
          (if (> kx km)
            (loop x kx (cdr xs))
            (loop m km (cdr xs)))))))

  (define-syntax time-exec
    (syntax-rules ()
      [(_ fns ...)
       (let* ([t1 (current-time)]
              [res (begin fns ...)]
              [t2 (current-time)]
              [diff (time-difference t2 t1)]
              [time-ms (exact->inexact (+ (* 1000 (time-second diff)) (/ (time-nanosecond diff) 1000000)))])
         (display "Time: ")
         (display time-ms)
         (display " ms")
         (newline)
         res)]))

  (define file->lines
    (case-lambda
      ([fname] (file->lines fname identity))
      ([fname mapper]
       (call-with-input-file fname
        (λ (p)
           (let loop ([line (get-line p)]
                      [acc '()])
             (if (eof-object? line)
               (reverse acc)
               (loop (get-line p) (cons line acc)))))))))

  (define file->string
    (case-lambda
      ([fname] (file->string fname identity))
      ([fname mapper]
       (call-with-input-file fname get-string-all))))

  (define string->lines
    (case-lambda
      ([s] (string->lines s identity))
      ([s mapper]
       (call-with-port (open-string-input-port s)
         (λ (p)
            (let loop ([line (get-line p)]
                       [acc '()])
              (if (eof-object? line)
                (reverse acc)
                (loop (get-line p) (cons line acc)))))))))

  (define (acons k v lst) (cons (cons k v) lst))

  (define (map* f . lsts)
    (let loop ([lsts lsts]
               [res '()])
      (if (exists null? lsts)
        (reverse res)
        (loop (map cdr lsts)
              (cons (apply f (map car lsts))
                    res)))))

  (define (map-indexed f lst)
    (define (map-indexed-helper lst idx)
      (if (null? lst)
        lst
        (cons (f idx (car lst)) (map-indexed-helper (cdr lst) (+ idx 1)))))
    (map-indexed-helper lst 0))

  (define (filter-map f lst)
    (if (null? lst)
      lst
      (if-let ([x (f (car lst))])
        (cons x (filter-map f (cdr lst)))
        (filter-map f (cdr lst)))))

  (define (mapcat f lst)
    (if (null? lst)
      '()
      (append (f (car lst)) (mapcat f (cdr lst)))))

  (define (count p lst)
    (let loop ([lst lst]
               [n 0])
      (cond
        [(null? lst) n]
        [(p (car lst)) (loop (cdr lst) (+ n 1))]
        [else (loop (cdr lst) n)])))

  (define (assv-get k lst)
    (and->> (assv k lst)
            (cdr)))

  (define (updatev k lst f)
    (cond
      [(null? lst) (list (f #f))]
      [(eqv? (caar lst) k)
       (cons (cons (caar lst) (f (cdar lst)))
             (cdr lst))]
      [else (cons (car lst) (updatev k (cdr lst) f))]))

  (define (vector-count p v)
    (define len (vector-length v))
    (let loop ([idx 0]
               [res 0])
      (cond
        [(>= idx len) res]
        [(p (vector-ref v idx)) (loop (+ idx 1) (+ res 1))]
        [else (loop (+ idx 1) res)])))

  (define (swap-args f)
    (λ (a b) (f b a)))

  (define (rcons a b) (cons b a))

  (define (vector-fold-left f init v . vs)
    (define len (apply min (map vector-length (cons v vs))))
    (let loop ([acc init]
               [idx 0])
      (if (>= idx len)
        acc
        (loop (apply f acc (map (λ (v) (vector-ref v idx)) (cons v vs)))
              (+ idx 1)))))

  (define (unfold f seed)
    (let loop ([acc (list seed)]
               [seed seed])
      (if-let ([next (f seed)])
              (loop (cons (car next) acc)
                    (cdr next))
              (reverse acc))))

  (define (string-trim-right s)
    (let loop ([idx (- (string-length s) 1)])
      (cond
        [(< idx 0) ""]
        [(char=? #\newline (string-ref s idx)) (loop (- idx 1))]
        [else (substring s 0 (+ idx 1))])))

  (define (dedup lst)
    (->> lst
         (fold-left (λ (ht x) (mut-> ht (hashtable-set! x #t)))
                    (make-hashtable equal-hash equal? 32))
         (hashtable-keys)))

  (define (vector-update! v idx f)
    (define x (vector-ref v idx))
    (vector-set! v idx (f x)))

  (define (take-while p lst)
    (if (or (null? lst) (not (p (car lst))))
      '()
      (cons (car lst) (take-while p (cdr lst)))))

  (define (list->hashset lst)
    (fold-left (λ (ht x) (mut-> ht (hashtable-set! x x)))
               (make-hashtable equal-hash equal? 32)
               lst))

  (define (hset-contains? ht x) (hashtable-ref ht x #f))
  (define (hset-incl! ht x) (hashtable-set! ht x x))

  (define (split-at n lst)
    (let loop ([n n]
               [head '()]
               [tail lst])
      (cond
        [(zero? n) (values (reverse head) tail)]
        [(null? tail) (values lst '())]
        [else (loop (- n 1) (cons (car tail) head) (cdr tail))])))

  (define (groups-of n lst)
    (let-values ([(h t) (split-at n lst)])
      (if (null? t)
        (cons h t)
        (cons h (groups-of n t)))))
  )

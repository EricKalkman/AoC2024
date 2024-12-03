#!r6rs

(library (util)
         (export λ -> ->> as-> and-> and->> mut-> if-let
                 partial
                 string-contains? string-contains-sub?
                 identity
                 compose
                 argmin
                 time-exec
                 file->lines
                 string->lines
                 acons
                 map*
                 )
         (import (rnrs)
                 (rnrs r5rs)
                 (only (chezscheme) current-time time-second time-nanosecond time-difference))

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
         (fn x args ...)
         (mut-> x fns ...))]))

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

)

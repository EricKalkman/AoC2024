#!r6rs

(library (parsecomb)
         (export string-buffer
                 make-string-buffer
                 %make-string-buffer
                 string-buffer-str
                 string-buffer-pos
                 string-buffer?
                 string-buffer-eof?
                 string-buffer-advance
                 string-buffer-has-input?
                 string-buffer-ref string-buffer-substring
                 parse-result make-parse-result parse-result?
                 parse-result-val parse-result-rest parse-result-err?
                 success failure success? failure?

                 eof

                 any-char char str charp charp+ charset charset+
                 choice seq maybe skip
                 repeat-before
                 skip-until
                 skip-before
                 pmap
                 many many+

                 parse-int

                 WHITESPACE ws skip-ws

                 list-of

                 rec
                 )
         (import (rnrs)
                 (util))

  (define-record-type
    (string-buffer %make-string-buffer string-buffer?)
    (fields
      (mutable str)
      (mutable pos)))

  (define-record-type
    (parse-result make-parse-result parse-result?)
    (fields
      (immutable val)
      (immutable rest)
      (immutable err?)))

  (define (make-string-buffer s) (%make-string-buffer s 0))
  (define (string-buffer-length buf) (string-length (string-buffer-str buf)))

  (define (string-buffer-eof? buf)
    (unless (string-buffer? buf)
      (error string-buffer-eof? "Expected string-buffer"))
    (= (string-buffer-pos buf) (string-buffer-length buf)))

  (define (string-buffer-advance n buf)
    (%make-string-buffer (string-buffer-str buf) (+ n (string-buffer-pos buf))))

  (define (string-buffer-has-input? n buf)
    (< (+ n (string-buffer-pos buf)) (string-length (string-buffer-str buf))))

  (define (string-buffer-ref buf idx)
    (define raw-idx (+ idx (string-buffer-pos buf)))
    (unless (<= 0 raw-idx (- (string-buffer-length buf) 1))
      (error 'string-buffer-ref "index out of bounds"))
    (string-ref (string-buffer-str buf) raw-idx))

  (define (string-buffer-substring buf start end)
    (define new-start (+ (string-buffer-pos buf) start))
    (define new-end (+ (string-buffer-pos buf) end))
    (unless (and (<= 0 new-start (- (string-buffer-length buf) 1))
                 (<= 0 new-end (string-buffer-length buf)))
      (error 'string-buffer-substring "string slice out of bounds"))
    (substring (string-buffer-str buf) new-start new-end))

  (define (success x buf)
    (make-parse-result x buf #f))

  (define (failure err expected buf)
    (make-parse-result expected buf err))

  (define (failure? pr) (parse-result-err? pr))
  (define (success? pr) (not (failure? pr)))

  (define (eof buf)
    (if (string-buffer-eof? buf)
      (success #f buf)
      (failure 'not-eof 'eof buf)))

  (define (any-char buf)
    (if (string-buffer-eof? buf)
      (failure 'eof #f buf)
      (success (string-buffer-ref buf 0) (string-buffer-advance 1 buf))))

  (define (char c)
    (λ (buf)
       (cond
         [(string-buffer-eof? buf) (failure 'eof #f buf)]
         [(char=? c (string-buffer-ref buf 0)) (success c (string-buffer-advance 1 buf))]
         [else (failure 'wrong-char c buf)])))

  (define (str s)
    (λ (buf)
       (cond
         [(not (string-buffer-has-input? (string-length s) buf)) (failure 'eof #f buf)]
         [(string=? s (string-buffer-substring buf 0 (string-length s)))
          (success s (string-buffer-advance (string-length s) buf))]
         [else (failure 'wrong-str s buf)])))

  (define (charp p)
    (λ (buf)
       (cond
         [(string-buffer-eof? buf) (failure 'eof #f buf)]
         [(p (string-buffer-ref buf 0))
          (success (string-buffer-ref buf 0) (string-buffer-advance 1 buf))]
         [else (failure 'wrong-char #f buf)])))

  (define (charset s) (charp (λ (c) (string-contains? s c))))

  (define (charp+ p)
    (λ (buf)
       (define remaining (- (string-buffer-length buf) (string-buffer-pos buf)))
       (if (string-buffer-eof? buf)
         (failure 'eof #f buf)
         (let loop ([idx 0])
           (if (or (>= idx remaining)
                   (not (p (string-buffer-ref buf idx))))
             (if (zero? idx)
               (failure 'wrong-char #f buf)
               (success (string-buffer-substring buf 0 idx)
                        (string-buffer-advance idx buf)))
             (loop (+ 1 idx)))))))

  (define (charset+ s) (charp+ (λ (c) (string-contains? s c))))

  (define (choice . ps)
    (λ (buf)
       (let loop ([ps ps])
         (if (null? ps)
           (failure 'no-successful-parser #f buf)
           (if-let ([res ((car ps) buf)]
                    [_ (success? res)])
             res
             (loop (cdr ps)))))))

  (define (seq . ps)
    (λ (buf)
       (let loop ([ps ps]
                  [buf buf]
                  [acc '()])
         (if (null? ps)
           (success (filter identity (reverse acc)) buf)
           (let ([res ((car ps) buf)])
             (if (success? res)
               (loop (cdr ps)
                     (parse-result-rest res)
                     (cons (parse-result-val res) acc))
               res))))))

  (define (repeat-before pred p)
    (λ (buf)
       (let loop ([tmpbuf buf]
                  [acc '()])
         (if (success? (pred tmpbuf))
           (success (reverse acc) tmpbuf)
           (let ([res (p tmpbuf)])
             (if (success? res)
               (loop (parse-result-rest res) (cons (parse-result-val res) acc))
               res))))))

  (define (skip-before pred p)
    (λ (buf)
       (let loop ([tmpbuf buf])
         (if (success? (pred tmpbuf))
           (success #f tmpbuf)
           (if-let ([res (p tmpbuf)]
                    [_ (success? res)])
             (loop (parse-result-rest res))
             res)))))

  (define (skip-until pred p)
    (λ (buf)
       (let loop ([tmpbuf buf])
         (if-let ([res (pred tmpbuf)]
                  [_ (success? res)])
           res
           (if-let ([res (p tmpbuf)]
                    [_ (success? res)])
             (loop (parse-result-rest res))
             res)))))

  (define (maybe p)
    (λ (buf)
       (define res (p buf))
       (if (success? res)
         res
         (success 'missing buf))))

  (define (skip p)
    (λ (buf)
       (define res (p buf))
       (if (success? res)
         (success #f (parse-result-rest res))
         res)))

  (define (pmap f p)
    (λ (buf)
       (define res (p buf))
       (if (success? res)
         (success (f (parse-result-val res)) (parse-result-rest res))
         res)))

  (define (many p)
    (λ (buf)
       (let loop ([acc '()]
                  [buf buf])
         (let ([res (p buf)])
           (if (success? res)
             (loop (cons (parse-result-val res) acc)
                   (parse-result-rest res))
             (success (reverse acc) buf))))))

  (define (many+ p)
    (->> (seq p (many p))
         (pmap (λ (res) (cons (car res) (cadr res))))))

  (define parse-int
    (->> (seq (maybe (char #\-)) (charset+ "0123456789"))
         (pmap (λ (lst)
                  (define num (string->number (cadr lst)))
                  (if (eqv? (car lst) 'missing) num (- num))))))

  (define WHITESPACE " \n\r\t")
  
  (define ws (charset+ WHITESPACE))
  (define skip-ws (skip ws))

  (define (list-of elem sep)
    (->> (seq elem (many+ (seq (skip sep) elem)))
         (pmap (λ (lst) (cons (car lst) (map car (cadr lst)))))))

  (define-syntax rec
    (syntax-rules ()
      [(_ name pexp) (letrec ([name pexp]) name)]))
)

(library (day18)
         (export parse-input find-path take-set
                 part-1-with-pars part-1
                 part-2-with-pars part-2)
         (import (rnrs)
                 (util)
                 (prefix (grids) g:)
                 (prefix (searches) search:)
                 (prefix (parsecomb) p:))

  (define coord (->> (p:seq p:parse-int (p:skip (p:char #\,)) p:parse-int)
                     (p:pmap (λ (lst) (apply g:make-point lst)))))
  (define parse-input (p:list-of coord p:nl))

  (define (trace-path dst src prevs)
    (and
      (hashtable-ref prevs dst #f)
      (let loop ([cur dst]
                 [lst '()])
        (if (equal? cur src)
          lst
          (loop (hashtable-ref prevs cur #f)
                (cons cur lst))))))

  (define (find-path coords w h src dst)
    (define res
      (search:bfs (λ (cur)
                     (->> (g:neighbors4 cur)
                          (filter (λ (n) (and (<= 0 (g:point-row n) (- w 1))
                                              (<= 0 (g:point-col n) (- h 1))
                                              (not (hashtable-ref coords n #f)))))))
                  src
                  (λ (cur) (equal? cur dst))))
    (->> res
         (assv-get 'prevs)
         (trace-path dst src)))

  (define (take-set lst n)
    (let loop ([n n]
               [lst lst]
               [h (make-hashtable equal-hash equal?)])
      (if (zero? n)
        h
        (loop (- n 1)
              (cdr lst)
              (mut-> h (hashtable-set! (car lst) #t))))))

  (define (part-1-with-pars w h n s)
    (-> s
        (p:make-string-buffer)
        (parse-input)
        (p:parse-result-unwrap)
        (take-set n)
        (find-path w h (g:make-point 0 0) (g:make-point (- w 1) (- h 1)))
        (length)))

  (define part-1 (partial part-1-with-pars 71 71 1024))

  (define (binary-search coords w h src dst lo hi)
    (if (<= hi lo)
      lo
      (let* ([n (div (+ lo hi) 2)]
             [cs (take-set coords n)])
        (if (find-path cs w h src dst)
          (binary-search coords w h src dst (+ n 1) hi)
          (binary-search coords w h src dst lo n)))))

  (define (part-2-with-pars w h s)
    (let ([coords (-> s (p:make-string-buffer) (parse-input) (p:parse-result-unwrap))])
      (as-> (binary-search coords w h (g:make-point 0 0)
                           (g:make-point (- w 1) (- h 1)) 0 (length coords)) $
          (list-ref coords (- $ 1))
          (string-append (number->string (g:point-row $)) "," (number->string (g:point-col $))))))

  (define part-2 (partial part-2-with-pars 71 71))

)
#|
(import (rnrs) (util) (prefix (parsecomb) p:) (day18))

(part-1-with-pars 7 7 12 (file->string "../inputs/day18.test")) ; 22
(part-1 (file->string "../inputs/day18.inp")) ; 446
(part-2-with-pars 7 7 test-inp) ; (6 . 1)
(part-2 (file->string "../inputs/day18.inp")) ; (39 . 40)
|#

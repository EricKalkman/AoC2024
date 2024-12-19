(library (day19)
         (export parse-input count-combinations ends-with? part-1 part-2)
         (import (rnrs)
                 (util)
                 (prefix (parsecomb) p:))

  (define parse-patterns (p:list-of (p:charset+ p:LOWERCASE)
                                    (p:str ", ")))
  (define parse-designs (p:list-of (p:charset+ p:LOWERCASE) p:nl))

  (define parse-input (->> (p:seq parse-patterns p:skip-nl p:skip-nl
                                  parse-designs)
                           (p:label '(patterns designs))))

  (define (vget v idx)
    (if (<= 0 (+ idx 1) (- (vector-length v) 1))
      (vector-ref v (+ idx 1))
      0))

  (define (vset! v idx x)
    (vector-set! v (+ idx 1) x)
    v)

  (define (ends-with? s sub idx)
    (if (> (string-length sub) (+ idx 1))
      #f
      (let loop ([i 0])
        (cond
          [(>= i (string-length sub)) #t]
          [(char=? (string-ref s (- idx i))
                   (string-ref sub (- (string-length sub) i 1)))
           (loop (+ i 1))]
          [else #f]))))

  (define (count-combinations pats design)
    (let loop ([idx 0]
               [counts (mut-> (make-vector (+ 1 (string-length design)) 0)
                              (vector-set! 0 1))])
      (if (>= idx (string-length design))
        (vget counts (- (string-length design) 1))
        (->> pats
             (filter (λ (p) (ends-with? design p idx)))
             (fold-left
               (λ (sum p)
                  (+ sum (vget counts (- idx (string-length p)))))
               0)
             (vset! counts idx)
             (loop (+ idx 1))))))

  (define (part-1 s)
    (define inp (->> s (p:make-string-buffer) (parse-input) (p:parse-result-unwrap)))
    (define pats (assv-get 'patterns inp))
    (define designs (assv-get 'designs inp))
    (->> designs
         (map (λ (d) (count-combinations pats d)))
         (count (λ (c) (not (zero? c))))))

  (define (part-2 s)
    (define inp (->> s (p:make-string-buffer) (parse-input) (p:parse-result-unwrap)))
    (define pats (assv-get 'patterns inp))
    (define designs (assv-get 'designs inp))
    (->> designs
         (fold-left
           (λ (sum d)
              (+ sum (count-combinations pats d)))
           0)))

)

#|
(import (rnrs) (day19) (prefix (parsecomb) p:) (util))

(define test-inp (file->string "../inputs/day19.test"))

(define parsed (->> test-inp (p:make-string-buffer) (parse-input) (p:parse-result-unwrap)))
(define pats (assv-get 'patterns parsed))
(define designs (assv-get 'designs parsed))

(part-1 test-inp) ; 6
(part-1 (file->string "../inputs/day19.inp")) ; 236
(part-2 test-inp) ; 16
(part-2 (file->string "../inputs/day19.inp")) ; 643685981770598
|#

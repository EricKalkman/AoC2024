(library (day03)
         (export part-1 part-2 part-2-clj)
         (import (rnrs)
                 (util)
                 (prefix (parsecomb) p:))

  (define parse-mul (p:seq (p:skip (p:str "mul("))
                           p:parse-int (p:skip (p:char #\,)) p:parse-int
                           (p:skip (p:char #\)))))

  (define parse-muls (p:many+ (p:skip-until parse-mul p:any-char)))

  (define (part-1 s)
    (->> (p:make-string-buffer s)
         (parse-muls)
         (p:parse-result-val)
         (map (λ (p) (apply * p)))
         (apply +)))

  (define parse-do (->> (p:seq (p:skip (p:str "do()"))
                               (p:repeat-before (p:choice p:eof (p:str "don't()"))
                                                (p:choice parse-mul (p:skip p:any-char))))
                        (p:pmap (λ (lst) (filter identity (car lst))))))

  (define parse-dont (p:skip (p:seq (p:str "don't()") (p:skip-before (p:choice p:eof (p:str "do()")) p:any-char))))

  (define parse-part-2 (p:many+ (p:choice parse-dont parse-do)))

  (define (part-2 s)
    (->> (string-append "do()" s)
         (p:make-string-buffer)
         (parse-part-2)
         (p:parse-result-val)
         (filter identity)  ; remove empty (don't()) slots
         (apply append)     ; combine the lists of mul's from each do() ... don't() run
         (map (λ (p) (apply * p)))
         (apply +)))

  (define parser-clj (p:many (p:skip-until (p:choice (->> (p:str "don't()") (p:pmap (λ (_) 'dont)))
                                                     (->> (p:str "do()") (p:pmap (λ (_) 'do)))
                                                     parse-mul)
                                           p:any-char)))

  (define (part-2-clj s)
    (let loop ([tokens (->> s (p:make-string-buffer) (parser-clj) (p:parse-result-val))]
               [state 'do]
               [sum 0])
      (cond
        [(null? tokens) sum]
        [(symbol? (car tokens)) (loop (cdr tokens) (car tokens) sum)]
        [(eqv? state 'do) (loop (cdr tokens) state (+ sum (* (caar tokens) (cadar tokens))))]
        [else (loop (cdr tokens) state sum)])))
)

#|
(import (util) (parsecomb) (day03))

(define test-inp "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(part-1 test-inp) ; 161
(part-1 (file->string "../inputs/day03.inp")) ; 182619815

(part-2 test-inp) ; 48
(part-2 (file->string "../inputs/day03.inp")) ; 80747545

(part-2-clj test-inp) ; 48
(part-2-clj (file->string "../inputs/day03.inp")) ; 80747545
|#

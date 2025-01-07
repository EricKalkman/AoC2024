(library (day13)
         (export part-1 part-2)
         (import (rnrs)
                 (util)
                 (prefix (parsecomb) p:)
                 (grids))

  (define (parse-coord sep)
    (->> (p:seq (p:skip (p:char #\X)) (p:skip (p:char sep)) p:parse-int (p:skip (p:str ", "))
                (p:skip (p:char #\Y)) (p:skip (p:char sep)) p:parse-int)
         (p:pmap (λ (lst) (make-point (car lst) (cadr lst))))))
  (define (parse-button label)
    (->> (p:seq (p:skip (p:str "Button ")) (p:skip (p:char label)) (p:skip (p:str ": "))
                (parse-coord #\+))
         (p:pmap (λ (lst) (cons label (car lst))))))
  (define parse-prize
    (->> (p:seq (p:skip (p:str "Prize: ")) (parse-coord #\=))
         (p:pmap (λ (lst) (cons 'prize (car lst))))))
  (define parse-claw
    (p:seq (parse-button #\A) p:skip-nl (parse-button #\B) p:skip-nl
           parse-prize))
  (define parse-input (p:list-of parse-claw (p:seq p:skip-nl p:skip-nl)))

  (define A-COST 3)
  (define B-COST 1)

  (define (make-inverse-mat a b)
    (let* ([ax (point-row a)]
           [ay (point-col a)]
           [bx (point-row b)]
           [by (point-col b)]
           [det (- (* ax by) (* bx ay))])
      (if (zero? det)
        #f
        (vector (/ by det) (/ (- bx) det)
                (/ (- ay) det) (/ ax det)))))

  (define (calc-na-nb mat prize)
    (define a (vector-ref mat 0))
    (define b (vector-ref mat 1))
    (define c (vector-ref mat 2))
    (define d (vector-ref mat 3))
    (define px (point-row prize))
    (define py (point-col prize))
    (make-point (+ (* a px) (* b py)) (+ (* c px) (* d py))))

  (define (num-coins-needed a b prize)
    (if-let ([mat (make-inverse-mat a b)])
      (let* ([na-nb (calc-na-nb mat prize)]
             [na (point-row na-nb)]
             [nb (point-col na-nb)])
        (if (and (integer? na) (integer? nb))
          (+ (* A-COST na) (* B-COST nb))
          0))
      #f))

  (define (reposition-prize k prize)
    (point+ (make-point k k) prize))

  (define (part k s)
    (->> s
         (p:parse parse-input)
         (p:parse-result-unwrap)
         (map (λ (inp)
                 (let ([a (assv-get #\A inp)]
                       [b (assv-get #\B inp)]
                       [prize (assv-get 'prize inp)])
                   (num-coins-needed a b (reposition-prize k prize)))))
         (apply +)))

  (define part-1 (partial part 0))
  (define part-2 (partial part 10000000000000))
)
#|
(import (rnrs) (util) (parsecomb) (day13))

(define test-inp (file->string "../inputs/day13.test"))
(define real-inp (file->string "../inputs/day13.inp"))

(part-1 test-inp) ; 480
(part-1 real-inp) ; 28262
(part-2 test-inp) ; 875318608908
(part-2 real-inp) ; 101406661266314
|#

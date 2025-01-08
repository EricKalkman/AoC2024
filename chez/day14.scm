(library (day14)
         (export part-1 part-2 unfold-rev)
         (import (rnrs)
                 (util)
                 (prefix (parsecomb) p:)
                 (grids))

  (define-record-type
    (robot make-robot robot?)
    (fields
      (immutable pos)
      (immutable vel)))

  (define (update-pos bot f)
    (make-robot (f (robot-pos bot)) (robot-vel bot)))

  (define (update-vel bot f)
    (make-robot (robot-pos bot) (f (robot-vel bot))))

  (define parse-coord
    (->> (p:seq p:parse-int (p:skip (p:char #\,)) p:parse-int)
         (p:pmap (λ (lst) (apply make-point lst)))))
  (define parse-robot
    (->> (p:seq (p:skip (p:str "p=")) parse-coord
                (p:skip (p:str " v=")) parse-coord)
         (p:pmap (λ (lst) (apply make-robot lst)))))
  (define parse-input (p:list-of parse-robot p:nl))

  (define (step-robot w h bot)
    (define vel (robot-vel bot))
    (define vx (point-row vel))
    (define vy (point-col vel))
    (update-pos bot
                (λ (pos)
                   (make-point (mod (+ vx (point-row pos)) w)
                               (mod (+ vy (point-col pos)) h)))))

  (define (step-system w h bots)
    (map (λ (bot) (step-robot w h bot)) bots))

  (define (unfold-rev n f seed)
    (let loop ([n (- n 1)]
               [acc (list seed)]
               [seed seed])
      (if (<= n 0)
        acc
        (let ([res (f seed)])
          (loop (- n 1)
                (cons res acc)
                res)))))

  (define (simulate w h n bots)
    (let loop ([idx n]
               [bots bots])
      (if (zero? idx)
        bots
        (loop (- idx 1) (step-system w h bots)))))

  (define (quadrant w h bot)
    (define pos (robot-pos bot))
    (define x (point-row pos))
    (define y (point-col pos))
    (define half-w (div w 2))
    (define half-h (div h 2))
    (if (or (= half-w x) (= half-h y))
      #f
      (+ (* 2 (div y (+ 1 half-h)))
         (div x (+ 1 half-w)))))

  (define (count-by n f lst)
    (define result (make-vector n 0))
    (let loop ([lst lst])
      (if (null? lst)
        result
        (begin
          (vector-update! result (f (car lst)) (λ (x) (+ 1 x)))
          (loop (cdr lst))))))

  (define WIDTH 101)
  (define HEIGHT 103)

  (define (part-1 s)
    (as-> s $
         (p:parse parse-input $)
         (p:parse-result-unwrap $)
         (simulate WIDTH HEIGHT 100 $)
         (count-by 5 (λ (bot) (or (quadrant WIDTH HEIGHT bot) 4)) $)
         (begin
           (vector-set! $ 4 1)
           $)
         (vector-fold-left * 1 $)))

  (define (entropies n bots)
    (define xs (count-by WIDTH (λ (bot) (point-row (robot-pos bot))) bots))
    (define ys (count-by HEIGHT (λ (bot) (point-col (robot-pos bot))) bots))
    (define ex (vector-fold-left
                 (λ (acc x) (if (zero? x) acc (- acc (* (/ x n) (log (/ x n))))))
                 0 xs))
    (define ey (vector-fold-left
                 (λ (acc y) (if (zero? y) acc (- acc (* (/ y n) (log (/ y n))))))
                 0 ys))
    (list ex ey))

  (define (mult-inverse x n)
    (let loop ([t 0]
               [new-t 1]
               [r n]
               [new-r x])
      (if (zero? new-r)
        (cond
          [(> r 1) #f]
          [(< t 0) (+ t n)]
          [else t])
        (let ([q (div r new-r)])
          (loop new-t (- t (* q new-t))
                new-r (- r (* q new-r)))))))

  (define (part-2 s)
    (define sims
      (->> s
           (p:parse parse-input)
           (p:parse-result-unwrap)
           (unfold-rev (max WIDTH HEIGHT)
                       (λ (bots) (step-system WIDTH HEIGHT bots)))
           (reverse)))
    (define n (length (car sims)))
    (define ents (->> (map (λ (bots) (entropies n bots)) sims)
                      (apply map list)))
    (define min-ex (apply argmin cdr (map-indexed cons (car ents))))
    (define min-ey (apply argmin cdr (map-indexed cons (cadr ents))))
    (define k (mod (* (mult-inverse WIDTH HEIGHT) (- (car min-ey) (car min-ex))) HEIGHT))
    (+ (car min-ex) (* k WIDTH)))

)
#|
(import (rnrs) (util) (parsecomb) (day14))

(define test-inp (file->string "../inputs/day14.test"))
(define real-inp (file->string "../inputs/day14.inp"))

(part-1 real-inp) ; 226548000
(part-2 real-inp) ; 7753
|#

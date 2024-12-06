(library (day05)
         (export parse-input right-order? part-1 part-2)
         (import (rnrs)
                 (rnrs r5rs) ; for quotient
                 (util)
                 (prefix (parsecomb) p:))

  (define (edges->graph edges)
    ; a graph is an adjacency list stored as a hashtable
    (->> edges
         (fold-left
           (λ (ht edge)
              (mut-> ht
                     (hashtable-update! (car edge) (λ (tos) (cons (cadr edge) tos)) '())
                     (hashtable-update! (cadr edge) identity '())))
           (make-eqv-hashtable))))

  (define parse-edge (p:seq p:parse-int (p:skip (p:char #\|)) p:parse-int))
  (define parse-rules (->> (p:list-of parse-edge (p:char #\newline))
                           (p:pmap edges->graph)))
  (define parse-pages (p:list-of p:parse-int (p:char #\,)))
  (define parse-updates (p:list-of parse-pages (p:char #\newline)))

  (define parse-input (->> (p:seq parse-rules (p:skip (p:str "\n\n")) parse-updates)
                           (p:label '(rules updates))))

  (define (right-order? rules a b)
    (memv b (hashtable-ref rules a #f)))

  (define (part-1 s)
    (define inp (->> s (p:make-string-buffer) (parse-input) (p:parse-result-unwrap)))
    (define rules (assv-get 'rules inp))
    (define updates (assv-get 'updates inp))
    (->> updates
         (filter (λ (pages)
                    (for-all (λ (p) (right-order? rules (car p) (cdr p)))
                             (map* cons pages (cdr pages)))))
         (map (λ (pages) (list-ref pages (quotient (length pages) 2))))
         (apply +)))

  (define (part-2 s)
    (define inp (->> s (p:make-string-buffer) (parse-input) (p:parse-result-unwrap)))
    (define rules (assv-get 'rules inp))
    (define updates (assv-get 'updates inp))
    (->> updates
         (filter (λ (pages)
                    (not (for-all (λ (p) (right-order? rules (car p) (cdr p)))
                                  (map* cons pages (cdr pages))))))
         (map (compose (λ (pages) (list-ref pages (quotient (length pages) 2)))
                       (λ (pages) (list-sort (λ (a b) (right-order? rules a b)) pages))))
         (apply +)))
)

#|
(import (day05) (rnrs) (util) (prefix (parsecomb) p:))

(define test-inp "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(right-order? rules (car updates))

(part-1 test-inp) ; 143
(part-1 (file->string "../inputs/day05.inp")) ; 4578

(part-2 test-inp) ; 123
(part-2 (file->string "../inputs/day05.inp")) ; 6179
|#

(library (searches)
         (export nop-visit bfs dijkstra)
         (import (rnrs)
                 (util)
                 (deques)
                 (binomial-heaps))

  (define nop-visit
    (case-lambda
      [() #f]
      [(prevs cur) nop-visit]))

  (define bfs
    (case-lambda
      [(neighfunc start stop?) (bfs neighfunc start stop? nop-visit)]
      [(neighfunc start stop? visit)
       (let loop ([q (mut-> (make-deque) (dq-enq! start))]
                  [prevs (mut-> (make-hashtable equal-hash equal?)
                                (hashtable-set! start start))]
                  [visit visit])
         (if (dq-empty? q)
           `((prevs . ,prevs) (visit . ,visit))
           (let ([cur (dq-deq! q)])
             (if (stop? cur)
               `((last . ,cur) (prevs . ,prevs) (visit . ,visit))
               (let ([neighs (->> (neighfunc cur)
                                  (filter (λ (n) (not (hashtable-ref prevs n #f)))))])
                 (for-each (λ (n)
                              (hashtable-set! prevs n cur)
                              (dq-enq! q n))
                           neighs)
                 (loop q prevs (visit prevs cur)))))))]))

  (define (dijkstra neighfunc start stop?)
    (define visited? (make-hashtable equal-hash equal? 32))
    (define prevs (make-hashtable equal-hash equal? 32))
    (let loop ([lasts #f]
               [pq (make-singleton-heap-by cdr (cons start 0))])
      (if (bin-heap-empty? pq)
        `((prevs . ,prevs) (lasts . ,lasts) (cost . ,(and lasts (cdar lasts))))
        (let-values ([(min-cost-node pq) (pop-min pq)])
          (let* ([n1 (car min-cost-node)]
                 [n1-cost (cdr min-cost-node)])
            (cond
              [(stop? n1) (loop (cons min-cost-node (or lasts '())) pq)]
              [(and lasts (= n1-cost (cdar lasts))) (loop lasts pq)]
              [lasts `((lasts . ,(map car lasts)) (cost . ,(cdar lasts)) (prevs . ,prevs))]
              [(hset-contains? visited? n1) (loop lasts pq)]
              [else
                (hset-incl! visited? n1)
                (->> (neighfunc n1)
                     (fold-left
                       (λ (costs node2)
                          (let* ([n2 (car node2)]
                                 [edge-cost (cdr node2)]
                                 [new-n2-cost (+ n1-cost edge-cost)])
                            (if-let ([prev (hashtable-ref prevs n2 #f)]
                                     [_ (<= (car prev) new-n2-cost)])
                              (begin
                                (when (= (car prev) new-n2-cost)
                                  (hashtable-update!
                                    prevs n2
                                    (λ (prev-data)
                                       ; TODO: remove
                                       (assert (not (member n1 (cdr prev-data))))
                                       (cons (car prev-data) (cons n1 (cdr prev-data))))
                                    #f))
                                costs)
                              (begin
                                (hashtable-set! prevs n2 (cons new-n2-cost (list n1)))
                                (push costs (cons n2 new-n2-cost))))))
                       pq)
                     (loop lasts))]))))))

  )

#|
(import (searches) (rnrs) (util) (binomial-heaps))

(define g '((a . ((b . 1) (d . 3)))
            (b . ((c . 1)))
            (c . ((d . 1)))
            (d . ((e . 1)))
            (e . ())))

(->> (dijkstra (λ (n) (assv-get n g)) 'a (λ _ #f))
     (assv-get 'prevs)
     (hashtable-entries))
|#

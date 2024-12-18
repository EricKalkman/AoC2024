(library (searches)
         (export nop-visit bfs)
         (import (rnrs)
                 (util)
                 (deques))

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

  )

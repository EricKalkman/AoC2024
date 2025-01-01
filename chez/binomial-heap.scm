(library (binomial-heap)
         (export 
                 bin-heap make-empty-bin-heap
                 make-empty-bin-heap-by
                 bin-heap-count
                 make-singleton-heap
                 make-singleton-heap-by
                 push
                 pop-min
                 min-elt
                 bin-heap-empty?)
         (import (rnrs)
                 (util))

  (define-record-type
    (bin-tree make-bin-tree bin-tree?)
    (fields
      (immutable x)
      (immutable key)
      (mutable order)
      ; pointer to the largest of this tree's children
      (mutable child)
      ; if tree is root in heap: points to next smallest tree
      ; if tree is not a root: points to next largest tree
      (mutable sibling)))

  (define (make-singleton-tree x k)
    (make-bin-tree x k 0 #f #f))

  (define (update-sibling t s)
    (make-bin-tree
      (bin-tree-x t)
      (bin-tree-key t)
      (bin-tree-order t)
      (bin-tree-child t)
      s))

  (define (copy-tree t)
    (and t
         (make-bin-tree
           (bin-tree-x t)
           (bin-tree-key t)
           (bin-tree-order t)
           (bin-tree-child t)
           (bin-tree-sibling t))))

  (define (bin-tree-rev-children t)
    (let loop ([child (bin-tree-child t)]
               [prev #f])
      (if child
        (loop (bin-tree-sibling child)
              (update-sibling child prev))
        prev)))

  (define (merge-trees p q)
    ; pure function, preserving min heap property
    ; always returns a new tree
    (assert (or (not p) (not q) (= (bin-tree-order p) (bin-tree-order q))))
    (cond
      [(not (and p q)) (copy-tree (or p q))]
      [(<= (bin-tree-key p) (bin-tree-key q))
       (make-bin-tree (bin-tree-x p)
                      (bin-tree-key p)
                      (+ 1 (bin-tree-order p))
                      (make-bin-tree (bin-tree-x q)
                                     (bin-tree-key q)
                                     (bin-tree-order q)
                                     (bin-tree-child q)
                                     (bin-tree-child p))
                      #f)]
      [else (merge-trees q p)]))

  (define-record-type
    ; roots of each binomial tree are stored in ascending order, with
    ; the smallest root's sibling pointer pointing to the next smallest
    ; and so on
    (bin-heap %make-bin-heap bin-heap?)
    (fields
      (mutable root)
      (mutable count)
      (immutable keyfn)))

  (define (bin-heap-empty? heap) (zero? (bin-heap-count heap)))

  (define (make-empty-bin-heap)
    (%make-bin-heap #f 0 identity))

  (define (make-empty-bin-heap-by keyfn)
    (%make-bin-heap #f 0 keyfn))

  (define (make-singleton-heap x)
    (%make-bin-heap (make-singleton-tree x x) 1 identity))

  (define (make-singleton-heap-by x keyfn)
    (%make-bin-heap (make-singleton-tree x (keyfn x)) 1 keyfn))

  (define (merge-roots r1 r2)
    (cond
      [(and (not r1) (not r2)) #f]
      [(not r1) r2]
      [(not r2) r1]
      [(= (bin-tree-order r1) (bin-tree-order r2))
       (let ([next-r1 (bin-tree-sibling r1)]
             [next-r2 (bin-tree-sibling r2)]
             [merged (merge-trees r1 r2)])
         (merge-roots (merge-roots next-r1 next-r2) merged))]
      [(> (bin-tree-order r1) (bin-tree-order r2))
       (merge-roots r2 r1)]
      [else
        (let* ([next-r1 (bin-tree-sibling r1)]
               [merged (merge-roots next-r1 r2)])
          (update-sibling r1 merged))]))

  (define (push h x)
    (%make-bin-heap
      (merge-roots (bin-heap-root h) (make-singleton-tree x ((bin-heap-keyfn h) x)))
      (+ 1 (bin-heap-count h))
      (bin-heap-keyfn h)))

  (define (fold-roots h f init)
    (let loop ([acc init]
               [root (bin-heap-root h)])
      (if root
        (loop (f acc (bin-tree-x root) (bin-tree-key root))
              (bin-tree-sibling root))
        acc)))

  (define (min-elt h)
    (->> (fold-roots
           h
           (λ (acc x k)
              (let ([acc-k (cdr acc)])
                (if (< k acc-k)
                  (cons x k)
                  acc)))
           (cons (bin-tree-x (bin-heap-root h))
                 (bin-tree-key (bin-heap-root h))))
         (car)))

  (define (remove-tree root t)
    (cond
      [(not root) #f]
      [(eq? root t) (bin-tree-sibling t)]
      [else (update-sibling root (remove-tree (bin-tree-sibling root) t))]))

  (define (pop-min h)
    (assert (not (bin-heap-empty? h)))
    (let* ([root (bin-heap-root h)]
           [min-root
             (let loop ([m root]
                        [root (bin-tree-sibling (bin-heap-root h))])
               (if root
                 (loop (if (< (bin-tree-key root) (bin-tree-key m))
                         root m)
                       (bin-tree-sibling root))
                 m))]
           [children (bin-tree-rev-children min-root)])
      (values (bin-tree-x min-root) 
              (%make-bin-heap
                (merge-roots (remove-tree root min-root)
                        children)
                (- (bin-heap-count h) 1)
                (bin-heap-keyfn h)))))
)
#|
(import (rnrs) (util) (binomial-heap))

(define nodes (map (λ (x) (cons x (random 1.0))) '(a b c d e f g h i j k l)))

(define sorted-nodes (sort (λ (a b) (> (cdr a) (cdr b))) nodes))

(define h (fold-left (λ (acc x) (push acc x)) (make-empty-bin-heap-by cdr)
                     nodes))

(define popped-nodes
  (let loop ([h h]
             [acc '()])
    (if (bin-heap-empty? h)
      (map car acc)
      (let-values ([(x h) (pop-min h)])
        (loop h (cons x acc))))))
|#

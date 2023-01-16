#lang racket

(struct tree-node (val left right))

(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (closest-nodes root queries)
  (-> (or/c tree-node? #f) (listof exact-integer?) (listof (listof exact-integer?)))
  (define (update-result v result)
    (for/list ([q queries]
               [r result])
      (list (if (<= v q)
                (if (= (car r) -1)
                    v
                    (max v (car r)))
                (car r))
            (if (>= v q)
                (if (= (cadr r) -1)
                    v
                    (min v (cadr r)))
                (cadr r)))))
  (define (iter node result)
    (cond [(false? node) result]
          [(and (false? (tree-node-left node))
                (false? (tree-node-right node)))
           (let ([v (tree-node-val node)])
             (update-result v result))]
          [else (update-result (tree-node-val node)
                               (iter (tree-node-left node)
                                     (iter (tree-node-right node) result)))]))
  (iter root (make-list (length queries) (list -1 -1))))

(define root (tree-node 6
                        (tree-node 2
                                   (tree-node 1 #f #f)
                                   (tree-node 4 #f #f))
                        (tree-node 13
                                   (tree-node 9 #f #f)
                                   (tree-node 15
                                              (tree-node 14 #f #f)
                                              #f))))

(closest-nodes root '(2 5 16))
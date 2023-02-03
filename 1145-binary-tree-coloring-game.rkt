#lang racket

(struct tree-node
  (val left right) #:mutable #:transparent)

(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define (count-tree node)
  (if node
      (+ 1
         (count-tree (tree-node-left node))
         (count-tree (tree-node-right node)))
      0))

(define/contract (btree-game-winning-move root n x)
  (-> (or/c tree-node? #f) exact-integer? exact-integer? boolean?)
  (define (iter node)
    (cond [(not node) #f]
          [(= x (tree-node-val node))
           (cons (count-tree (tree-node-left node))
                 (count-tree (tree-node-right node)))]
          [else
           (or (iter (tree-node-left node))
               (iter (tree-node-right node)))]))
  (define win-num (exact-ceiling (/ (add1 n) 2)))
  (match (iter root)
    [(cons a b) (or (>= a win-num) (>= b win-num) (>= (- n a b 1) win-num))]))

(count-tree (tree-node 1 (make-tree-node 2) (make-tree-node 3)))
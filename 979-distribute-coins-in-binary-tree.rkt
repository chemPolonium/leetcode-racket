#lang racket

; Definition for a binary tree node.

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (distribute-coins root)
  (-> (or/c tree-node? #f) exact-integer?)
  (define (node-rest-steps node)
    (cond [(not node) (values 0 0)]
          [else
           (define-values (rl sl) (node-rest-steps (tree-node-left node)))
           (define-values (rr sr) (node-rest-steps (tree-node-right node)))
           (values (+ rl rr (tree-node-val node) -1)
                   (+ sl sr (abs rl) (abs rr)))]))
  (let-values ([(_ s) (node-rest-steps root)])
    s))

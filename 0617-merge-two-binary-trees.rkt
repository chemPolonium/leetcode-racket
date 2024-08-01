#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (merge-trees root1 root2)
  (-> (or/c tree-node? #f) (or/c tree-node? #f) (or/c tree-node? #f))
  (cond [(not root1) root2]
        [(not root2) root1]
        [else (tree-node (+ (tree-node-val root1) (tree-node-val root2))
                         (merge-trees (tree-node-left root1) (tree-node-left root2))
                         (merge-trees (tree-node-right root1) (tree-node-right root2)))]))

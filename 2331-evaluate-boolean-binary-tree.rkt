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

(define/contract (evaluate-tree root)
  (-> (or/c tree-node? #f) boolean?)
  (match (tree-node-val root)
    [0 #f]
    [1 #t]
    [2 (or (evaluate-tree (tree-node-left root)) (evaluate-tree (tree-node-right root)))]
    [3 (and (evaluate-tree (tree-node-left root)) (evaluate-tree (tree-node-right root)))]))
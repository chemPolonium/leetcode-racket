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

(define/contract (good-nodes root)
  (-> (or/c tree-node? #f) exact-integer?)
  (let loop ([root root] [m -114514])
    (cond [(not root) 0]
          [(> m (tree-node-val root))
           (+ (loop (tree-node-left root) m) (loop (tree-node-right root) m))]
          [else
           (define v (tree-node-val root))
           (+ 1 (loop (tree-node-left root) v) (loop (tree-node-right root) v))])))

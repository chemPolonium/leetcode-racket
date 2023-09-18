#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (rob root)
  (-> (or/c tree-node? #f) exact-integer?)
  (define (rob-1 root)
    (cond [(not root) (values 0 0)]
          [else
           (define-values (with-left without-left) (rob-1 (tree-node-left root)))
           (define-values (with-right without-right) (rob-1 (tree-node-right root)))
           (define with-this (+ (tree-node-val root) without-left without-right))
           (define without-this (+ (max with-left without-left)
                                   (max with-right without-right)))
           (values with-this without-this)]))
  (define-values (with-root without-root) (rob-1 root))
  (max with-root without-root))

(rob (tree-node 3 (tree-node 2 #f (tree-node 3 #f #f)) (tree-node 3 #f (tree-node 1 #f #f))))

(rob (tree-node 1 (tree-node 2 #f #f) #f))

(rob (tree-node 4 (tree-node 1 (tree-node 2 (tree-node 3 #f #f) #f) #f) #f))

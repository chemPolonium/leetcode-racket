#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (lca-deepest-leaves root)
  (-> (or/c tree-node? #f) (or/c tree-node? #f))
  (define (f1 root)
    (define left (f (tree-node-left root)))
    (define right (f (tree-node-right root)))
    (cond [(> (cdr left) (cdr right)) (cons (car left) (add1 (cdr left)))]
          [(< (cdr left) (cdr right)) (cons (car right) (add1 (cdr right)))]
          [else (cons root (add1 (cdr left)))]))
  (define (f root)
    (if root
        (f1 root)
        (cons root 0)))
  (car (f root)))

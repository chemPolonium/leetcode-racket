#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(require data/gvector)

(define/contract (max-product root)
  (-> (or/c tree-node? #f) exact-integer?)
  (define v (gvector))
  (let f ([node root])
    (cond [node
           (define s (+ (tree-node-val node)
                        (f (tree-node-left node))
                        (f (tree-node-right node))))
           (gvector-add! v s)
           s]
          [else 0]))
  (define s (gvector-ref v (sub1 (gvector-count v))))
  (define a (vector-argmin (lambda (x) (abs (- x (/ s 2)))) (gvector->vector v)))
  (remainder (* a (- s a)) 1000000007))

#lang racket

(require data/gvector)

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (max-level-sum root)
  (-> (or/c tree-node? #f) exact-integer?)
  (define v (gvector))
  (let f ([node root] [level 0])
    (when node
      (when (= (gvector-count v) level)
        (gvector-add! v 0))
      (gvector-set! v level (+ (tree-node-val node) (gvector-ref v level)))
      (f (tree-node-left node) (add1 level))
      (f (tree-node-right node) (add1 level))))
  (add1 (argmax (lambda (i) (gvector-ref v i)) (range (gvector-count v)))))

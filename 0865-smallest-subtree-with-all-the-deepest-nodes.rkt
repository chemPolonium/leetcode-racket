#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (subtree-with-all-deepest root)
  (-> (or/c tree-node? #f) (or/c tree-node? #f))
  (define (f2 root)
    (if root
        (let-values ([(l ld) (f2 (tree-node-left root))]
                     [(r rd) (f2 (tree-node-right root))])
          (cond [(> ld rd) (values l (add1 ld))]
                [(< ld rd) (values r (add1 rd))]
                [else (values root (add1 ld))]))
        (values root 0)))
  (define-values (r _) (f2 root))
  r)

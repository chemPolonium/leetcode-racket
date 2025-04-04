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

(define/contract (lca-deepest-leaves-1 root)
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

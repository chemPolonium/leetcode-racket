#lang racket

(require data/gvector)

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

(define find-elements%
  (class object%
    (super-new)

    ; root : (or/c tree-node? #f)
    (init-field
     root)

    (define s (mutable-seteq))

    (let iter ([r root] [i 0])
      (when r
        (set-add! s i)
        (iter (tree-node-left r) (+ 1 (* 2 i)))
        (iter (tree-node-right r) (+ 2 (* 2 i)))))

    ; find : exact-integer? -> boolean?
    (define/public (find target)
      (set-member? s target))))

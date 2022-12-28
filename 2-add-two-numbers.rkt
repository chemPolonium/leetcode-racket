#lang racket

; Definition for singly-linked list:

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define/contract (add-two-numbers l1 l2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))
  (let recr ([l1 l1] [l2 l2] [c 0])
    (cond [(and (not l1) (not l2)) (if (positive? c) (make-list-node c) false)]
          [(not l2) (recr l1 (make-list-node) c)]
          [(not l1) (recr (make-list-node) l2 c)]
          [else
           (let-values
               ([(q r) (quotient/remainder
                        (+ (list-node-val l1)
                           (list-node-val l2)
                           c)
                        10)])
             (list-node r
                        (recr (list-node-next l1)
                              (list-node-next l2)
                              q)))])))

(add-two-numbers (list-node 2 (list-node 4 (list-node 3 #f)))
                 (list-node 5 (list-node 6 (list-node 4 #f))))
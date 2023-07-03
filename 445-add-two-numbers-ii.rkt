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
  (define (list->num l)
    (let iter ([x 0] [l l])
      (if l
          (iter (+ (* 10 x) (list-node-val l)) (list-node-next l))
          x)))
  (define (num->list n)
    (define (num->reverse n)
      (let iter ([n n] [l null])
        (define-values (q r) (quotient/remainder n 10))
        (define new-l (cons r l))
        (if (zero? q)
            (reverse new-l)
            (iter q new-l))))
    (define (reverse->list l)
      (let iter ([l l] [ll #f])
        (if (null? l)
            ll
            (iter (cdr l) (list-node (car l) ll)))))
    (if (zero? n)
        (make-list-node 0)
        (reverse->list (num->reverse n))))
  (num->list (+ (list->num l1) (list->num l2))))
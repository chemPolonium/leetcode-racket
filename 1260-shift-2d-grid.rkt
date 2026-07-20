#lang racket

(define/contract (shift-grid grid k)
  (-> (listof (listof exact-integer?)) exact-integer? (listof (listof exact-integer?)))
  (define m (length grid))
  (define n (length (car grid)))
  (define k1 (- (* m n) (remainder k (* m n))))
  (define-values (l r) (split-at (flatten grid) k1))
  (append r l)
  (let iter ([a null] [ll (append r l)])
    (cond [(null? ll) (reverse a)]
          [else
           (define-values (aa ls) (split-at ll n))
           (iter (cons aa a) ls)])))

(shift-grid '((1 2 3) (4 5 6) (7 8 9)) 1)

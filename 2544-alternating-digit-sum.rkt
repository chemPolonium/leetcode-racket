#lang racket

(define/contract (alternate-digit-sum n)
  (-> exact-integer? exact-integer?)
  (let iter ([n n] [s 0] [p +])
    (define-values (q r) (quotient/remainder n 10))
    (if (zero? q) (p (p s r)) (iter q (p s r) (if (eq? p +) - +)))))
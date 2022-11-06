#lang racket

(define/contract (sort-array-by-parity nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (sort nums (lambda (x y) (even? x))))

(sort-array-by-parity '(3 1 2 4))
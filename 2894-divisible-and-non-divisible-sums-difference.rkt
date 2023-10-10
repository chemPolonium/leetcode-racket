#lang racket

(define/contract (difference-of-sums n m)
  (-> exact-integer? exact-integer? exact-integer?)
  (define (sum1 n) (/ (* n (add1 n)) 2))
  (define num2 (* m (sum1 (quotient n m))))
  (define num1 (- (sum1 n) num2))
  (- num1 num2))

(difference-of-sums 10 3)
(difference-of-sums 5 6)
(difference-of-sums 5 1)

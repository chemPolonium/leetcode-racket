#lang racket

(define/contract (sum-of-multiples n)
  (-> exact-integer? exact-integer?)
  (define (sum-1-to-n n) (/ (* n (add1 n)) 2))
  (define (sum-mul x) (* x (sum-1-to-n (quotient n x))))
  (- (apply + (map sum-mul '(3 5 7 105)))
     (apply + (map sum-mul '(15 21 35)))))

#lang racket

(define/contract (pivot-integer n)
  (-> exact-integer? exact-integer?)
  (let ([i (filter (lambda (x)
                     (= (* (+ n x) (- (add1 n) x))
                        (* x (add1 x))))
                   (range 1 (add1 n)))])
    (if (null? i) -1 (car i))))
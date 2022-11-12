#lang racket

(define/contract (num-tilings n)
  (-> exact-integer? exact-integer?)
  (define m 1000000007)
  (define (iter i x y z w)
    (if (= i n)
        w
        (iter (add1 i)
              w
              (remainder (+ x z) m)
              (remainder (+ x y) m)
              (remainder (+ x y z w) m))))
  (iter 0 0 0 0 1))

(num-tilings 3)
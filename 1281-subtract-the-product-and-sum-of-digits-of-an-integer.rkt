#lang racket

(define/contract (subtract-product-and-sum n)
  (-> exact-integer? exact-integer?)
  (let iter ([p 1] [s 0] [n n])
    (if (zero? n)
        (- p s)
        (let-values ([(q r) (quotient/remainder n 10)])
          (iter (* p r) (+ s r) q)))))

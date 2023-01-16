#lang racket

(define/contract (number-of-cuts n)
  (-> exact-integer? exact-integer?)
  (cond [(= n 1) 0]
        [(zero? (remainder n 2)) (/ n 2)]
        [else n]))
#lang racket

(define/contract (ways-to-buy-pens-pencils total cost1 cost2)
  (-> exact-integer? exact-integer? exact-integer? exact-integer?)
  (for/sum ([i (in-inclusive-range 0 (quotient total cost1))])
    (add1 (quotient (- total (* i cost1)) cost2))))

(ways-to-buy-pens-pencils 20 10 5)

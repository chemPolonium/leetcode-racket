#lang racket

(define/contract (fill-cups amount)
  (-> (listof exact-integer?) exact-integer?)
  (match (sort amount <)
    [(list x y z) (if (< (+ x y) z) z (quotient (+ x y z 1) 2))]))
#lang racket

(define/contract (find-numbers nums)
  (-> (listof exact-integer?) exact-integer?)
  (count (lambda (x) (even? (ceiling (log (add1 x) 10)))) nums))

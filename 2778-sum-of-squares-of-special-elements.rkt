#lang racket

(define/contract (sum-of-squares nums)
  (-> (listof exact-integer?) exact-integer?)
  (define n (length nums))
  (for/sum ([(ni i) (in-indexed nums)] #:when (zero? (remainder n (add1 i))))
    (* ni ni)))

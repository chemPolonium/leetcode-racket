#lang racket

(define/contract (permute nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (permutations nums))
#lang racket

(require math/number-theory)

(define/contract (common-factors a b)
  (-> exact-integer? exact-integer? exact-integer?)
  (length (divisors (gcd a b))))
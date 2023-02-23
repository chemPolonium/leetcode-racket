#lang racket

(define/contract (circular-permutation n start)
  (-> exact-integer? exact-integer? (listof exact-integer?))
  (for/fold ([l null] #:result (reverse l))
            ([i (in-range (expt 2 n))])
    (define g (bitwise-xor i (quotient i 2)))
    (cons (bitwise-xor g start) l)))

(circular-permutation 3 2)
#lang racket

(define/contract (k-items-with-maximum-sum numOnes numZeros numNegOnes k)
  (-> exact-integer? exact-integer? exact-integer? exact-integer? exact-integer?)
  (if (<= k numOnes)
      k
      (let ([k (- k numOnes)])
        (if (<= k numZeros)
            numOnes
            (let ([k (- k numZeros)])
              (- numOnes k))))))
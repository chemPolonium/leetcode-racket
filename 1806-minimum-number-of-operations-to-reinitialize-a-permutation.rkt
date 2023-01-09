#lang racket

(define/contract (reinitialize-permutation n)
  (-> exact-integer? exact-integer?)
  (match n
    [2 1]
    [n (let iter ([p 1] [k 1])
         (if (= (remainder (* 2 p) (- n 1)) 1)
             k
             (iter (* 2 p) (add1 k))))]))

(reinitialize-permutation 4)
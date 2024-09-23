#lang racket

(define/contract (can-jump nums)
  (-> (listof exact-integer?) boolean?)
  (let iter ([nums nums] [i 0] [s 0])
    (and (>= s i)
         (or (null? (cdr nums))
             (iter (cdr nums) (add1 i) (max s (+ (car nums) i)))))))

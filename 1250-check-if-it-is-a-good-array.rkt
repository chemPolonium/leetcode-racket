#lang racket

(define/contract (is-good-array nums)
  (-> (listof exact-integer?) boolean?)
  (let iter ([x (car nums)] [xs (cdr nums)])
    (cond [(= 1 x) #t]
          [(null? xs) #f]
          [else (iter (gcd x (car xs)) (cdr xs))])))
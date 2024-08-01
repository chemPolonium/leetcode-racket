#lang racket

(define/contract (is-ideal-permutation nums)
  (-> (listof exact-integer?) boolean?)
  (define (iter m x res)
    (cond [(null? res) true]
          [(< (car res) m) false]
          [else (iter (max m x) (car res) (cdr res))]))
  (iter -1 (car nums) (cdr nums)))

(is-ideal-permutation '(1 2 0))
#lang racket

(define/contract (smallest-range-i nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (max 0 (- (apply max nums) (apply min nums) (* k 2))))

#lang racket

(define/contract (single-number nums)
  (-> (listof exact-integer?) exact-integer?)
  (foldl bitwise-xor 0 nums))

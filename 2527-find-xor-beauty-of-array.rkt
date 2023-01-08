#lang racket

(define/contract (xor-beauty nums)
  (-> (listof exact-integer?) exact-integer?)
  (foldl bitwise-xor 0 nums))
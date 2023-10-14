#lang racket

(define/contract (single-number nums)
  (-> (listof exact-integer?) exact-integer?)
  (apply bitwise-xor nums))

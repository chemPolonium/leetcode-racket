#lang racket

(define/contract (smallest-even-multiple n)
  (-> exact-integer? exact-integer?)
  (if (even? n) n (* n 2)))
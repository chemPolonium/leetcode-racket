#lang racket

(define/contract (can-win-nim n)
  (-> exact-integer? boolean?)
  (positive-integer? (remainder n 4)))

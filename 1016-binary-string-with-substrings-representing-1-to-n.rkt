#lang racket

(define/contract (query-string s n)
  (-> string? exact-integer? boolean?)
  (for/and ([i (in-range 1 (add1 n))])
    (string-contains? s (number->string i 2))))
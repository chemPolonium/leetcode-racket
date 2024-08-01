#lang racket

(define/contract (num-jewels-in-stones jewels stones)
  (-> string? string? exact-integer?)
  (for/sum ([c (in-string stones)])
    (if (string-contains? jewels (string c)) 1 0)))

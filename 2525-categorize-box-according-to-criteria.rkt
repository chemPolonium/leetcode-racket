#lang racket

(define/contract (categorize-box length width height mass)
  (-> exact-integer? exact-integer? exact-integer? exact-integer? string?)
  (define bulky (or (>= (max length width height) 10000)
                    (>= (* length width height) 1000000000)))
  (define heavy (>= mass 100))
  (cond [(and bulky heavy) "Both"]
        [bulky "Bulky"]
        [heavy "Heavy"]
        [else "Neither"]))
#lang racket

(define (single-query a b [res 1])
  (cond [(< a b) (single-query a (quotient b 2) (add1 res))]
        [(> a b) (single-query (quotient a 2) b (add1 res))]
        [else res]))

(define/contract (cycle-length-queries n queries)
  (-> exact-integer? (listof (listof exact-integer?)) (listof exact-integer?))
  (map (lambda (x) (match x [(list a b) (single-query a b)])) queries))

(cycle-length-queries 5 '[[17 21] [23 5] [15 7] [3 21] [31 9] [5 15] [11 2] [19 7]])
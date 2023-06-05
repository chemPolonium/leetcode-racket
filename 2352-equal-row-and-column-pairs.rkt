#lang racket

(define/contract (equal-pairs grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define rows grid)
  (define cols (apply map list grid))
  (for*/sum ([r (in-list rows)]
             [c (in-list cols)])
    (if (equal? r c) 1 0)))
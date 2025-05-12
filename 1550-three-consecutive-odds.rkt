#lang racket

(define/contract (three-consecutive-odds arr)
  (-> (listof exact-integer?) boolean?)
  (and (>= (length arr) 3)
       (for/or ([a (in-list arr)]
                [b (in-list (cdr arr))]
                [c (in-list (cddr arr))])
         (and (odd? a) (odd? b) (odd? c)))))

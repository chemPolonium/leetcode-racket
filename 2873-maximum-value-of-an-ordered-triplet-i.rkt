#lang racket

(define/contract (maximum-triplet-value nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([m1 -1]
             [m2 -2]
             [prev-max-num -2]
             [prev-max-diff -1]
             [prev-max-value -1]
             #:result (max 0 prev-max-value))
            ([num (in-list nums)])
    (define current-max-num (max prev-max-num m2))
    (define current-max-diff (max prev-max-diff (- current-max-num m1)))
    (define current-max-value (max prev-max-value (* current-max-diff num)))
    (values num m1 current-max-num current-max-diff current-max-value)))

(maximum-triplet-value '(12 6 1 2 7))
(maximum-triplet-value '(1 10 3 4 19))
(maximum-triplet-value '(1 2 3))

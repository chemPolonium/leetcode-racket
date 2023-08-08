#lang racket

(define/contract (max-absolute-sum nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([s-max 0] [s-min 0] [t-max 0] [t-min 0] #:result (max s-max (- s-min)))
            ([i (in-list nums)])
    (define n-t-max (max (+ t-max i) 0))
    (define n-t-min (min (+ t-min i) 0))
    (values (max s-max n-t-max) (min s-min n-t-min) n-t-max n-t-min)))

(max-absolute-sum '(1 -3 2 3 -4))

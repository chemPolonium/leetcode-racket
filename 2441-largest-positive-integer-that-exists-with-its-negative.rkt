#lang racket

(define/contract (find-max-k nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([s (set)] [m -1] #:result m)
            ([n (in-list nums)])
    (values (set-add s n)
            (if (set-member? s (- n))
                (max m (abs n))
                m))))
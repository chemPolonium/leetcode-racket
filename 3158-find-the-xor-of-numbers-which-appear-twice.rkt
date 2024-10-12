#lang racket

(define/contract (duplicate-numbers-xor nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([s (set)] [a 0] #:result a)
            ([ni (in-list nums)])
    (if (set-member? s ni)
        (values (set-add s ni) (bitwise-xor a ni))
        (values (set-add s ni) a))))

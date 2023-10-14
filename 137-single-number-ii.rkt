#lang racket

(define/contract (single-number nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([a 0] [b 0] #:result (bitwise-xor a b))
            ([n (in-list nums)])
    (values (bitwise-and (bitwise-xor a b) (bitwise-xor a n))
            (bitwise-and (bitwise-not a) (bitwise-xor b n)))))

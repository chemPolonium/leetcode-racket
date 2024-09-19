#lang racket

(define/contract (longest-continuous-substring s)
  (-> string? exact-integer?)
  (for/fold ([prev-c (string-ref s 0)] [m 1] [a 1] #:result a)
            ([c (sequence-tail (in-string s) 1)])
    (if (= (add1 (char->integer prev-c)) (char->integer c))
        (values c (add1 m) (max (add1 m) a))
        (values c 1 a))))

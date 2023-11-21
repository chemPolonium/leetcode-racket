#lang racket

(define/contract (min-deletion nums)
  (-> (listof exact-integer?) exact-integer?)
  (define n (length nums))
  (for/fold ([c 0] [p -1] #:result (+ (- n c) (if (odd? c) 1 0)))
            ([ni (in-list nums)])
    (if (= p ni)
        (values c p)
        (values (add1 c) (if (odd? c) -1 ni)))))

(min-deletion '(1 1 2 3 5))
(min-deletion '(1 1 2 2 3 3))

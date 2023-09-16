#lang racket

(define/contract (rob nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([with-prev 0]
             [without-prev 0]
             #:result (max with-prev without-prev))
            ([num (in-list nums)])
    (values (+ num without-prev)
            (max without-prev with-prev))))

(rob '(1 2 3 1))
(rob '(2 7 9 3 1))

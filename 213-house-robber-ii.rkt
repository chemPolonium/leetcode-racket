#lang racket

(define/contract (rob nums)
  (-> (listof exact-integer?) exact-integer?)
  (define (rob1 nums)
    (for/fold ([with-prev 0]
               [without-prev 0]
               #:result (max with-prev without-prev))
              ([num (in-list nums)])
      (values (+ num without-prev)
              (max without-prev with-prev))))
  (if (null? (cdr nums))
      (car nums)
      (max (rob1 (cdr nums)) (rob1 (cdr (reverse nums))))))

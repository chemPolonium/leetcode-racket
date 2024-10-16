#lang racket

(define/contract (minimum-average nums)
  (-> (listof exact-integer?) flonum?)
  (define v (list->vector (sort nums <)))
  (for/fold ([m 100] #:result (real->double-flonum m))
            ([i (in-naturals)]
             [j (in-range (sub1 (vector-length v)) -1 -1)]
             #:break (<= j i))
    (min m (/ (+ (vector-ref v i) (vector-ref v j)) 2))))

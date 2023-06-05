#lang racket

(define/contract (apply-operations nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define vec (list->vector nums))
  (for ([i (in-range (sub1 (vector-length vec)))])
    (when (= (vector-ref vec i) (vector-ref vec (add1 i)))
      (vector-set! vec i (* 2 (vector-ref vec i)))
      (vector-set! vec (add1 i) 0)))
  (define result-vec (make-vector (vector-length vec)))
  (vector-copy! result-vec 0 (vector-filter (negate zero?) vec))
  (vector->list result-vec))
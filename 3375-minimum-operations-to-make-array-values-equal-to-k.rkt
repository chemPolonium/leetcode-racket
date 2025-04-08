#lang racket

(define/contract (min-operations nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define uni-nums (remove-duplicates nums =))
  (define m (apply min uni-nums))
  (cond [(< m k) -1]
        [(= m k) (sub1 (length uni-nums))]
        [else (length uni-nums)]))

#lang racket

(define/contract (maximum-energy energy k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define v (list->vector energy))
  (for ([i (in-range (- (vector-length v) k 1) -1 -1)])
    (vector-set! v i (+ (vector-ref v i) (vector-ref v (+ i k)))))
  (apply max (vector->list v)))

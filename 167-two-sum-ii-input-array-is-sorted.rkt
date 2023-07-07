#lang racket

(define/contract (two-sum numbers target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define v (list->vector numbers))
  (let iter ([i 0] [j (sub1 (vector-length v))])
    (define s (+ (vector-ref v i) (vector-ref v j)))
    (cond [(= s target) (list (add1 i) (add1 j))]
          [(< s target) (iter (add1 i) j)]
          [(> s target) (iter i (sub1 j))])))
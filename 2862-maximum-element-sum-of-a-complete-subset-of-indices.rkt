#lang racket

(define/contract (maximum-sum nums)
  (-> (listof exact-integer?) exact-integer?)
  (define v (list->vector nums))
  (define l (vector-length v))
  (define (subsum max-r c)
    (for/sum ([i (in-inclusive-range 1 max-r)])
      (vector-ref v (sub1 (* i i c)))))
  (for*/fold ([m 0])
             ([max-r (in-inclusive-range 1 (integer-sqrt l))]
              [c (in-inclusive-range 1 (quotient l (* max-r max-r)))])
    (max m (subsum max-r c))))

(maximum-sum '(8 7 3 5 7 2 4 9))

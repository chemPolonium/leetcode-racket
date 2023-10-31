#lang racket

(define/contract (min-increment-operations nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define (cost n)
    (max 0 (- k n)))
  (match-define (list-rest n3 n2 n1 r) nums)
  (for/fold ([s1 (cost n1)]
             [s2 (cost n2)]
             [s3 (cost n3)]
             #:result (min s1 s2 s3))
            ([n (in-list r)])
    (values (+ (cost n) (min s1 s2 s3)) s1 s2)))

(min-increment-operations '(2 3 0 0 2) 4)
(min-increment-operations '(0 1 3 3) 5)
(min-increment-operations '(1 1 2) 1)

#lang racket

(define/contract (find-even-numbers digits)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define h (foldl (lambda (x h) (hash-update h x add1 0)) (hash) digits))
  (for*/list ([k1 (in-range 1 10)]
              #:when (hash-has-key? h k1)
              #:do [(define h1 (hash-update h k1 sub1))]
              [k2 (in-range 10)]
              #:when (positive-integer? (hash-ref h1 k2 0))
              #:do [(define h2 (hash-update h1 k2 sub1))]
              [k3 (in-range 0 10 2)]
              #:when (positive-integer? (hash-ref h2 k3 0)))
    (+ (* 10 (+ (* 10 k1) k2)) k3)))

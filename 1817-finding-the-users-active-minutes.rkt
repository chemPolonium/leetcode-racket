#lang racket

(define/contract (finding-users-active-minutes logs k)
  (-> (listof (listof exact-integer?)) exact-integer? (listof exact-integer?))
  (define v (make-vector k))
  (define h
    (for/fold ([h (hash)])
              ([q (in-list logs)])
      (match q
        [(list u t) (hash-update h u (lambda (s) (set-add s t)) (set))])))
  (for ([s (in-hash-values h)])
    (let ([user-time (sub1 (set-count s))])
      (vector-set! v user-time (add1 (vector-ref v user-time)))))
  (vector->list v))

(finding-users-active-minutes '[[0 5] [1 2] [0 2] [0 5] [1 3]] 5)
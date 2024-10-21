#lang racket

(define/contract (smallest-range-ii nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (for/fold ([hi (apply max nums)]
             [lo 10001]
             [m 10001]
             #:result m)
            ([ni (in-list (sort nums <))])
    (define nni (+ ni (* 2 k)))
    (values (max hi nni) (min lo nni) (min m (- hi (min lo ni))))))

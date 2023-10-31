#lang racket

(define/contract (find-k-or nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (for/fold ([s 0])
            ([i (in-range 32)])
    (define x (expt 2 i))
    (if (<= k (count (λ (n) (positive? (bitwise-and x n))) nums))
        (+ x s)
        s)))

(find-k-or '(7 12 9 8 9 15) 4)
(find-k-or '(2 12 1 11 4 5) 6)
(find-k-or '(10 8 5 9 11 6 8) 1)

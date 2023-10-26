#lang racket

(define/contract (max-area h w horizontalCuts verticalCuts)
  (-> exact-integer? exact-integer? (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define (max-diff l)
    (for/fold ([m 0])
              ([i (in-list l)]
               [j (in-list (cdr l))])
      (max m (- j i))))
  (remainder
   (* (max-diff (sort (list* 0 h horizontalCuts) <))
      (max-diff (sort (list* 0 w verticalCuts) <)))
   1000000007))

(max-area 5 4 '(1 2 4) '(1 3))

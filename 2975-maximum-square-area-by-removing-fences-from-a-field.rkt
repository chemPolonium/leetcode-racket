#lang racket

(define/contract (maximize-square-area m n hFences vFences)
  (-> exact-integer? exact-integer? (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define (bars lst end)
    (for*/seteq ([ld (in-list (cons 1 lst))]
                 [lu (in-list (cons end lst))]
                 #:when (< ld lu))
      (remainder (- lu ld) 1000000007)))
  (define intersect (set-intersect (bars hFences m) (bars vFences n)))
  (if (zero? (set-count intersect))
      -1
      (remainder (sqr (apply max (set->list intersect)))
                 1000000007)))

(maximize-square-area 4 3 '(2 3) '(2))
(maximize-square-area 6 7 '(2) '(4))

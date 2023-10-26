#lang racket

(define/contract (count-digits num)
  (-> exact-integer? exact-integer?)
  (define (number->digits num [l null])
    (define-values (q r) (quotient/remainder num 10))
    (if (zero? num)
        l
        (number->digits q (cons r l))))
  (count (λ (x) (and (positive? x)
                     (zero? (remainder num x))))
         (number->digits num)))

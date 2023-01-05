#lang racket

(define/contract (count-even num)
  (-> exact-integer? exact-integer?)
  (define (sum-of-digits x)
    (if (< x 10)
        x
        (let-values ([(q r) (quotient/remainder x 10)])
          (+ r (sum-of-digits q)))))
  (if (even? (sum-of-digits num))
      (exact-floor (/ num 2))
      (exact-floor (/ (sub1 num) 2))))
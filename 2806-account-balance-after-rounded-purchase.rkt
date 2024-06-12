#lang racket

(define/contract (account-balance-after-purchase purchaseAmount)
  (-> exact-integer? exact-integer?)
  (define-values (q r) (quotient/remainder purchaseAmount 10))
  (- 100 (* 10 (+ q (if (>= r 5) 1 0)))))

(account-balance-after-purchase 9)
(account-balance-after-purchase 15)

#lang racket

(define/contract (pass-the-pillow n time)
  (-> exact-integer? exact-integer? exact-integer?)
  (define r (remainder time (* 2 (sub1 n))))
  (if (> r (sub1 n))
      (- (* 2 n) r 1)
      (add1 r)))

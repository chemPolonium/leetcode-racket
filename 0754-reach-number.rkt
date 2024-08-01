#lang racket

(define/contract (reach-number target)
  (-> exact-integer? exact-integer?)
  (define (iter k target)
    (cond ((> target 0) (iter (add1 k) (- target k 1)))
          ((even? target) k)
          ((even? k) (add1 k))
          (else (+ k 2))))
  (iter 0 (abs target)))

(reach-number 2)
(reach-number 3)
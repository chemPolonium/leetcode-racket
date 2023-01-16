#lang racket

(define (sum-of-digits x)
  (if (< x 10)
      x
      (let-values ([(q r) (quotient/remainder x 10)])
        (+ r (sum-of-digits q)))))

(define/contract (difference-of-sum nums)
  (-> (listof exact-integer?) exact-integer?)
  (- (apply + nums)
     (apply + (map sum-of-digits nums))))
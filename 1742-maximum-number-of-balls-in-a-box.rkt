#lang racket

(define (sum-of-digits x)
  (if (< x 10)
      x
      (let-values ([(q r) (quotient/remainder x 10)])
        (+ r (sum-of-digits q)))))

(define/contract (count-balls lowLimit highLimit)
  (-> exact-integer? exact-integer? exact-integer?)
  (for/fold ([m 0])
            ([v (hash-values (for/fold ([h (hash)])
                                       ([i (in-range lowLimit (add1 highLimit))])
                               (hash-update h (sum-of-digits i) add1 0)))])
    (max m v)))

(count-balls 11 104)
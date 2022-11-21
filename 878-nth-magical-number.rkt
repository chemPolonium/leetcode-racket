#lang racket

(define/contract (nth-magical-number n a b)
  (-> exact-integer? exact-integer? exact-integer? exact-integer?)
  (define (mod x) (remainder x 1000000007))
  (let ([d (gcd a b)])
    (let ([a/d (/ a d)]
          [b/d (/ b d)])
      (let ([m (* a/d b)])
        (let-values ([(nm r) (quotient/remainder n (+ a/d b/d -1))])
          (mod (+ (* nm m)
                  (min (* a (ceiling (/ (* r b/d) (+ a/d b/d))))
                       (* b (ceiling (/ (* r a/d) (+ a/d b/d))))))))))))

(nth-magical-number 4 2 3)

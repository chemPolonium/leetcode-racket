#lang racket

(define/contract (count-beautiful-pairs nums)
  (-> (listof exact-integer?) exact-integer?)
  (define (last-num n)
    (remainder n 10))
  (define (first-num n)
    (if (< n 10)
        n
        (first-num (quotient n 10))))
  (define cnt (make-vector 10))
  (for/sum ([x (in-list nums)])
    (define res
      (for/sum ([y (in-range 1 10)]
                #:when (= 1 (gcd (last-num x) y)))
        (vector-ref cnt y)))
    (define x-first (first-num x))
    (vector-set! cnt x-first (add1 (vector-ref cnt x-first)))
    res))

(count-beautiful-pairs '(2 5 1 4))

#lang racket

(define/contract (count-interesting-subarrays nums modulo k)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (for/fold ([p 0] [r 0] [h (hasheq 0 1)] #:result r)
            ([num (in-list nums)])
    (define np (+ p (if (= k (remainder num modulo)) 1 0)))
    (define nr (+ r (hash-ref h (remainder (+ np (- k) modulo) modulo) 0)))
    (define nh (hash-update h (remainder np modulo) add1 0))
    (values np nr nh)))

(count-interesting-subarrays '(3 2 4) 2 1)

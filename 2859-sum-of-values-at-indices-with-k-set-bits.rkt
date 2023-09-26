#lang racket

(define/contract (sum-indices-with-k-set-bits nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define (bits n)
    (cond [(= n 0) 0]
          [(= n 1) 1]
          [else
           (define-values (q r) (quotient/remainder n 2))
           (+ r (bits q))]))
  (for/sum ([(n i) (in-indexed (in-list nums))]
            #:when (= k (bits i)))
    n))

(sum-indices-with-k-set-bits '(5 10 1 5 2) 1)
(sum-indices-with-k-set-bits '(4 3 2 1) 2)

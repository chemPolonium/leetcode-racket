#lang racket

(define/contract (number-of-pairs nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define h (foldl (λ (x h) (hash-update h x add1 0)) (hash) nums))
  (define-values (p r)
    (for/fold ([p 0] [r 0])
              ([n (in-hash-values h)])
      (define-values (ip ir) (quotient/remainder n 2))
      (values (+ p ip) (+ r ir))))
  (list p r))

(number-of-pairs '[1 3 2 1 3 2 2])
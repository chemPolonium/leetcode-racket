#lang racket

(define/contract (max-subarray-sum-circular nums)
  (-> (listof exact-integer?) exact-integer?)
  (define s (apply + nums))
  (define (dp select)
    (for/fold ([dpi (car nums)] [m (car nums)] #:result m)
              ([ni (in-list (cdr nums))])
      (define new-dpi (+ ni (select 0 dpi)))
      (values new-dpi (select new-dpi m))))
  (define max-pdp (dp max))
  (define min-ndp (dp min))
  (if (negative? max-pdp)
      max-pdp
      (max max-pdp (- s min-ndp))))

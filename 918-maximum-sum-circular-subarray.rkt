#lang racket

(define/contract (max-subarray-sum-circular nums)
  (-> (listof exact-integer?) exact-integer?)
  (define s (apply + nums))
  (define (dp select)
    (foldl (lambda (ni r)
             (define i (+ ni (select 0 (car r))))
             (cons i (select i (cdr r))))
           (cons (car nums) (car nums))
           (cdr nums)))
  (define max-pdp (cdr (dp max)))
  (define min-ndp (cdr (dp min)))
  (if (andmap negative? nums)
      max-pdp
      (max max-pdp (- s min-ndp))))

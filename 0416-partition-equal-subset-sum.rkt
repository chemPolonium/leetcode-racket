#lang racket

(define/contract (can-partition nums)
  (-> (listof exact-integer?) boolean?)
  (define t (/ (apply + nums) 2))
  (define st (mutable-seteq))
  (set-add! st (car nums))
  (for ([num (in-list (cdr nums))])
    (map (lambda (x) (when (<= x t) (set-add! st x)))
         (map (lambda (x) (+ x num)) (set->list st))))
  (set-member? st t))

(can-partition '(1 5 11 5))

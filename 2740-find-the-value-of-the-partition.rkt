#lang racket

(define/contract (find-value-of-partition nums)
  (-> (listof exact-integer?) exact-integer?)
  (define sorted (sort nums <))
  (define diff
    (for/list ([a (in-list sorted)]
               [b (in-list (cdr sorted))])
      (- b a)))
  (apply min diff))

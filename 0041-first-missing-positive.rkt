#lang racket

(define/contract (first-missing-positive nums)
  (-> (listof exact-integer?) exact-integer?)
  (foldl (lambda (num res)
           (if (= res num) (+ res 1) res)) 1 (sort nums <)))
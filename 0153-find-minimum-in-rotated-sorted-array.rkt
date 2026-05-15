#lang racket

(define/contract (find-min nums)
  (-> (listof exact-integer?) exact-integer?)
  (apply min nums))

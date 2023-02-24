#lang racket

(define/contract (minimum-operations nums)
  (-> (listof exact-integer?) exact-integer?)
  (set-count (set-remove (list->set nums) 0)))
#lang racket

(define/contract (matrix-sum nums)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (apply + (apply map max (map (λ (l) (sort l <)) nums))))
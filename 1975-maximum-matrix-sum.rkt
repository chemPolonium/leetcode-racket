#lang racket

(define/contract (max-matrix-sum matrix)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define l (flatten matrix))
  (- (apply + (map abs l))
     (if (even? (count nonpositive-integer? l))
         0
         (* 2 (apply min (map abs l))))))

#lang racket

(define/contract (check-x-matrix grid)
  (-> (listof (listof exact-integer?)) boolean?)
  (define s (sub1 (length grid)))
  (for*/and ([(l i) (in-indexed grid)]
             [(n j) (in-indexed l)])
    (xor (or (= i j) (= s (+ i j)))
         (zero? n))))
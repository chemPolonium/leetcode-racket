#lang racket

(define/contract (diagonal-sum mat)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (for*/sum ([(r i) (in-indexed (in-list mat))]
             [(c j) (in-indexed (in-list r))])
    (if (or (= i j) (= (sub1 (length mat)) (+ i j))) c 0)))

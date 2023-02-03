#lang racket

(define/contract (get-maximum-consecutive coins)
  (-> (listof exact-integer?) exact-integer?)
  (let iter ([l (sort coins <)] [n 0])
    (cond [(or (empty? l) (< (add1 n) (car l))) (add1 n)]
          [else (iter (cdr l) (+ n (car l)))])))
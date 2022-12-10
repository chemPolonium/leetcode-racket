#lang racket

(define/contract (min-operations nums)
  (-> (listof exact-integer?) exact-integer?)
  (let iter ([m (car nums)] [res (cdr nums)] [s 0])
    (cond [(null? res) s]
          [(<= (car res) m) (iter (add1 m) (cdr res) (+ s (- (add1 m) (car res))))]
          [else (iter (car res) (cdr res) s)])))

(min-operations '[1 5 2 4 1])
#lang racket

(define/contract (check nums)
  (-> (listof exact-integer?) boolean?)
  (define (iter1 i l)
    (cond [(null? l) true]
          [(<= i (car l)) (iter1 (car l) (cdr l))]
          [else (iter2 (car l) (cdr l))]))
  (define (iter2 i l)
    (cond [(null? l) (<= i (car nums))]
          [(<= i (car l)) (iter2 (car l) (cdr l))]
          [else false]))
  (iter1 (car nums) (cdr nums)))

(check '(1 2 3 4 5))

(check '(3 4 5 1 2))

(check '(2 1 3 4))
#lang racket

(define/contract (are-numbers-ascending s)
  (-> string? boolean?)
  (define l (filter-map string->number (string-split s)))
  (let iter ([l l] [i 0])
    (cond [(null? l) true]
          [(< i (car l)) (iter (cdr l) (car l))]
          [else false])))

(are-numbers-ascending "1 box has 3 blue 4 red 6 green and 12 yellow marbles")
(are-numbers-ascending "hello world 5 x 5")
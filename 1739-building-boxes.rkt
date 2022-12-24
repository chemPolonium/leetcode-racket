#lang racket

(define/contract (minimum-boxes n)
  (-> exact-integer? exact-integer?)
  (let iter ([i 0] [j 1] [k 1] [b 0])
    (cond [(>= i n) b]
          [(= j k) (iter (+ i j) 1 (add1 k) (add1 b))]
          [else (iter (+ i j) (add1 j) k (add1 b))])))

(minimum-boxes 10)
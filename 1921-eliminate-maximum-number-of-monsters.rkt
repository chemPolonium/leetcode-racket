#lang racket

(define/contract (eliminate-maximum dist speed)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (let iter ([t 0] [t1 (sort (map / dist speed) <)])
    (cond [(null? t1) t]
          [(< t (car t1)) (iter (add1 t) (cdr t1))]
          [else t])))

(eliminate-maximum '(1 3 4) '(1 1 1))

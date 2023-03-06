#lang racket

(define/contract (min-operations-max-profit customers boardingCost runningCost)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (let iter ([income 0] [rem 0] [customers customers] [t 0] [max-income 0] [max-t -1])
    (if (and (null? customers) (zero? rem))
        (if (and (> income max-income) (> income 0)) t max-t)
        (let* ([c-rem (+ rem (if (pair? customers) (car customers) 0))]
               [new-income (+ income (* (min c-rem 4) boardingCost) (- runningCost))]
               [new-rem (max (- c-rem 4) 0)]
               [new-customers (if (pair? customers) (cdr customers) null)]
               [new-max-income (max income max-income)]
               [new-max-t (if (and (> income max-income) (> income 0)) t max-t)])
          (iter new-income new-rem new-customers (add1 t) new-max-income new-max-t)))))

; (min-operations-max-profit '(8 3) 5 6)
; (min-operations-max-profit '(10 9 6) 6 4)
(min-operations-max-profit '(10 10 6 4 7) 3 8)
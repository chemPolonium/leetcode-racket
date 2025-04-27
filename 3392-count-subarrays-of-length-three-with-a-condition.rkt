#lang racket

(define/contract (count-subarrays nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/sum ([num0 (in-list nums)]
            [num1 (in-list (cdr nums))]
            [num2 (in-list (cddr nums))])
    (if (= (+ num0 num2) (/ num1 2)) 1 0)))

#lang racket

(define/contract (max-alternating-sum nums)
  (-> (listof exact-integer?) exact-integer?)
  (let iter ([nums (cdr nums)] [v0 (car nums)] [v1 0])
    (if (null? nums)
        v0
        (iter (cdr nums)
              (max v0 (+ v1 (car nums)))
              (max v1 (- v0 (car nums)))))))

#lang racket

(define/contract (nth-person-gets-nth-seat n)
  (-> exact-integer? flonum?)
  (if (= n 1) 1.0 0.5))
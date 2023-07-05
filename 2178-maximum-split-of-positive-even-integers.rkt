#lang racket

(define/contract (maximum-even-split finalSum)
  (-> exact-integer? (listof exact-integer?))
  (if (odd? finalSum)
      null
      (let iter ([l null] [n 2] [s finalSum])
        (if (<= n s)
            (iter (cons n l) (+ n 2) (- s n))
            (cons (+ (car l) s) (cdr l))))))

(maximum-even-split 12)
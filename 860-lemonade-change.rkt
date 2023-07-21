#lang racket

(define/contract (lemonade-change bills)
  (-> (listof exact-integer?) boolean?)
  (let iter ([five 0] [ten 0] [bills bills])
    (cond [(null? bills) true]
          [(= 5 (car bills)) (iter (add1 five) ten (cdr bills))]
          [(= 10 (car bills)) (and (positive? five)
                                   (iter (sub1 five) (add1 ten) (cdr bills)))]
          [else
           (if (positive? ten)
               (and (positive? five)
                    (iter (sub1 five) (sub1 ten) (cdr bills)))
               (and (>= five 3)
                    (iter (- five 3) ten (cdr bills))))])))

#lang racket

(define/contract (maximum-units boxTypes truckSize)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer?)
  (define (iter b s sum)
    (if (null? b)
        sum
        (let ([ib (first b)])
          (if (> (first ib) s)
              (+ sum (* s (second ib)))
              (iter (rest b)
                    (- s (first ib))
                    (+ sum (apply * ib)))))))
  (iter (sort boxTypes
              (lambda (x y)
                (> (second x) (second y))))
        truckSize
        0))

(maximum-units '[[1 3] [2 2] [3 1]] 4)
#lang racket

(define/contract (min-rectangles-to-cover-points points w)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer?)
  (define xs (sort (remove-duplicates (map car points)) <))
  (let iter ([xs xs] [xp (sub1 (- w))] [c 0])
    (if (null? xs)
        c
        (let ([x (car xs)])
          (if (<= x (+ xp w))
              (iter (cdr xs) xp c)
              (iter (cdr xs) x (add1 c)))))))

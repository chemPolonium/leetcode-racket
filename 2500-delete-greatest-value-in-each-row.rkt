#lang racket

(define (delete-greatest-value grid)
  (let iter ([g (map (lambda (x) (sort x <)) grid)]
             [res 0])
    (if (null? (car g))
        res
        (iter (map cdr g) (+ res (apply max (map car g)))))))

(delete-greatest-value '[[1 2 4] [3 3 1]])
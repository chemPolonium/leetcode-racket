#lang racket

(define/contract (nearest-valid-point x y points)
  (-> exact-integer? exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (define (iter i j d res)
    (if (null? res)
        j
        (match (car res)
          [(list ix iy)
           (if (or (= x ix) (= y iy))
               (let ([nd (+ (abs (- x ix)) (abs (- y iy)))])
                 (if (< nd d)
                     (iter (add1 i) i nd (cdr res))
                     (iter (add1 i) j d (cdr res))))
               (iter (add1 i) j d (cdr res)))])))
  (iter 0 -1 +inf.0 points))

(nearest-valid-point 3 4 '[[1 2] [3 1] [2 4] [2 3] [4 4]])
(nearest-valid-point 3 4 '((3 4)))
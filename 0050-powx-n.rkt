#lang racket

(define (square x) (* x x))

(define/contract (my-pow x n)
  (-> flonum? exact-integer? flonum?)
  (cond [(< n 0) (/ 1.0 (my-pow x (- n)))]
        ((= n 0) 1.0)
        ((even? n) (square (my-pow x (/ n 2))))
        (else (* x (my-pow x (- n 1))))))

(my-pow 5.0 3)

(my-pow 5.0 -2)

(my-pow 0.44528 0)
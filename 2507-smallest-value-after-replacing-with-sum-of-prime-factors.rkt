#lang racket

(require math/number-theory)

(define/contract (smallest-value n)
  (-> exact-integer? exact-integer?)
  (let ([i (apply + (map (lambda (x) (* (car x) (cadr x))) (factorize n)))])
    (if (= n i)
        i
        (smallest-value i))))

(smallest-value 15)
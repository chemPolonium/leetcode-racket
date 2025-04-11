#lang racket

(define/contract (count-symmetric-integers low high)
  (-> exact-integer? exact-integer? exact-integer?)
  (define (symmetric? x)
    (define s (number->string x))
    (define n (string-length s))
    (define l (map char->integer (string->list s)))
    (and (even? n) (= (apply + (take l (/ n 2))) (/ (apply + l) 2))))
  (count symmetric? (inclusive-range low high)))

#lang racket

(define/contract (split-num num)
  (-> exact-integer? exact-integer?)
  (define (number->digits num [l null])
    (define-values (q r) (quotient/remainder num 10))
    (if (zero? num)
        l
        (number->digits q (cons r l))))
  (let iter ([l (sort (filter positive? (number->digits num)) <)]
             [a 0]
             [b 0]
             [i #f])
    (cond [(null? l) (+ a b)]
          [i (iter (cdr l) (+ (car l) (* a 10)) b #f)]
          [else (iter (cdr l) a (+ (car l) (* b 10)) #t)])))

#lang racket

(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (let iter ([l nums] [h (hash)] [i 0])
    (if (hash-has-key? h (car l))
        (list (hash-ref h (car l)) i)
        (iter (cdr l) (hash-set h (- target (car l)) i) (add1 i)))))

(two-sum '(2 7 11 15) 9)
#lang racket

(define/contract (maximum-score a b c)
  (-> exact-integer? exact-integer? exact-integer? exact-integer?)
  (match (sort (list a b c) <)
    [(list a b c)
     (if (<= (+ a b) c)
         (+ a b)
         (let ([s (exact-ceiling (/ (- (+ a b) c) 2))])
           (+ s (maximum-score (- a s) (- b s) c))))]))
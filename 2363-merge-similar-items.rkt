#lang racket

(define/contract (merge-similar-items items1 items2)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (define h
    (for/fold ([h (hasheq)])
              ([i (sequence-append (in-list items1) (in-list items2))])
      (define k (first i))
      (define v (second i))
      (hash-update h k (λ (x) (+ x v)) 0)))
  (map (λ (x) (list (car x) (cdr x))) (sort (hash->list h) < #:key car)))

(merge-similar-items '((1 1) (4 5) (3 8)) '((3 1) (1 5)))
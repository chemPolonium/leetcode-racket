#lang racket

(define/contract (num-pairs-divisible-by60 time)
  (-> (listof exact-integer?) exact-integer?)
  (define h (make-hash))
  (for ([t (in-list time)])
    (hash-update! h (remainder t 60) add1 0))
  (for/sum ([(k v) (in-hash h)])
    (cond [(or (= k 0) (= k 30)) (/ (* v (sub1 v)) 2)]
          [(< k 30) (* v (hash-ref h (- 60 k) 0))]
          [else 0])))
#lang racket

(define/contract (num-rabbits answers)
  (-> (listof exact-integer?) exact-integer?)
  (for/sum ([(k v) (in-hash (foldl (lambda (x h) (hash-update h x add1 0)) (hasheq) answers))])
    (* (add1 k) (quotient (+ v k) (add1 k)))))

(num-rabbits '(1 1 2))
(num-rabbits '(10 10 10))
(num-rabbits '(1 1 1))
(num-rabbits '(1 1 0))

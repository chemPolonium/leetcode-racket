#lang racket

(define/contract (count-triplets nums)
  (-> (listof exact-integer?) exact-integer?)
  (define v (make-vector (expt 2 16)))
  (for* ([i (in-list nums)]
         [j (in-list nums)])
    (define ijand (bitwise-and i j))
    (vector-set! v ijand (add1 (vector-ref v ijand))))
  (for*/sum ([k (in-list nums)]
             [ijand (in-range (expt 2 16))])
    (if (zero? (bitwise-and ijand k)) (vector-ref v ijand) 0)))

(count-triplets '(2 1 3))
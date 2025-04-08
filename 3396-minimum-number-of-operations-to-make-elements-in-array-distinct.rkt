#lang racket

(define/contract (minimum-operations nums)
  (-> (listof exact-integer?) exact-integer?)
  (define m (make-vector 101 #f))
  (define v (list->vector nums))
  (let iter ([i (sub1 (vector-length v))])
    (if (or (= i -1) (vector-ref m (vector-ref v i)))
        (quotient (+ i 3) 3)
        (begin
          (vector-set! m (vector-ref v i) #t)
          (iter (sub1 i))))))

(minimum-operations '(6 7 8 9))
(minimum-operations '(1 2 3 4 2 3 3 5 7))

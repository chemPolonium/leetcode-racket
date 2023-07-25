#lang racket

(define/contract (is-good nums)
  (-> (listof exact-integer?) boolean?)
  (define n (length nums))
  (define v (make-vector n))
  (let iter ([nums nums])
    (cond [(null? nums) true]
          [(<= n (car nums)) false]
          [(= (sub1 n) (car nums))
           (vector-set! v (sub1 (car nums)) (add1 (vector-ref v (sub1 (car nums)))))
           (and (>= 2 (vector-ref v (sub1 n)))
                (iter (cdr nums)))]
          [else
           (vector-set! v (sub1 (car nums)) (add1 (vector-ref v (sub1 (car nums)))))
           (and (>= 1 (vector-ref v (sub1 (car nums))))
                (iter (cdr nums)))])))

(is-good '(3 4 4 1 2 1))

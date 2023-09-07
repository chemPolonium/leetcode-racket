#lang racket

(define/contract (repair-cars ranks cars)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define (time->cars t)
    (for/sum ([r (in-list ranks)])
      (integer-sqrt (quotient t r))))
  (define (search t1 t2)
    (define t12 (quotient (+ t1 t2) 2))
    (define c12 (time->cars t12))
    (cond [(= t1 t2) t1]
          [(>= c12 cars) (search t1 t12)]
          [else (search (add1 t12) t2)]))
  (search 1 (* cars cars (first ranks))))

(repair-cars '(4 2 3 1) 10)

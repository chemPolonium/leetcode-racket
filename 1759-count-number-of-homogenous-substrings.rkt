#lang racket

(define (homo-count l)
  (let iter ([l l] [c false] [i 0])
    (cond [(null? l) (list i)]
          [(false? c) (iter (cdr l) (car l) 1)]
          [(eq? c (car l)) (iter (cdr l) c (add1 i))]
          [else (cons i (iter (cdr l) (car l) 1))])))

(define (all-count l)
  (if (pair? l)
      (remainder (+ ((lambda (x) (/ (* x (add1 x)) 2)) (car l))
                    (all-count (cdr l)))
                 1000000007)
      0))

(define/contract (count-homogenous s)
  (-> string? exact-integer?)
  (all-count (homo-count (string->list s))))

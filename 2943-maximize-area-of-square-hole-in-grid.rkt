#lang racket

(define/contract (maximize-square-hole-area n m hBars vBars)
  (-> exact-integer? exact-integer? (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define (f l)
    (let iter ([l (sort l <)] [h 0] [c 0] [m 0])
      (cond [(null? l) m]
            [(= (add1 h) (car l)) (iter (cdr l) (car l) (add1 c) (max m (add1 c)))]
            [else (iter (cdr l) (car l) 1 (max m 1))])))
  (sqr (add1 (min (f hBars) (f vBars)))))

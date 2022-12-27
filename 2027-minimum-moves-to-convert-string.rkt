#lang racket

(define/contract (minimum-moves s)
  (-> string? exact-integer?)
  (let iter ([l (string->list s)] [i 0] [r 0])
    (cond [(null? l) r]
          [(positive? i) (iter (cdr l) (sub1 i) r)]
          [(eq? #\X (car l)) (iter (cdr l) 2 (add1 r))]
          [else (iter (cdr l) i r)])))

(minimum-moves "XXX")
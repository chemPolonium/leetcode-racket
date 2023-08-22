#lang racket

(define/contract (max-dist-to-closest seats)
  (-> (listof exact-integer?) exact-integer?)
  (define (dist1 l [dis null] [c 114514])
    (cond [(null? l) dis]
          [(zero? (car l))
           (dist1 (cdr l) (cons (add1 c) dis) (add1 c))]
          [else
           (dist1 (cdr l) (cons 0 dis) 0)]))
  (apply max (map min (reverse (dist1 seats)) (dist1 (reverse seats)))))

(max-dist-to-closest '(1 0 0 0 1 0 1))

#lang racket

(define/contract (sum-subseq-widths nums)
  (-> (listof exact-integer?) exact-integer?)
  (define ns (sort nums <))
  (define (mod x) (modulo x 1000000007))
  (define (iter x y res nres)
    (if (null? nres)
        res
        (iter (mod (+ (car nres) (* x 2)))
              (mod (* y 2))
              (mod (- (+ res (* (car nres) (sub1 y))) x))
              (cdr nres))))
  (iter (car ns) 2 0 (cdr ns)))

(sum-subseq-widths '(2 1 3))
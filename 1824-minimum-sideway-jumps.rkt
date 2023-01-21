#lang racket

(define (jumps obstacle prev-jumps)
  (define masked-jumps
    (if (zero? obstacle) prev-jumps (list-set prev-jumps (sub1 obstacle) 1919810)))
  (define free-jumps
    (map min
         (map (lambda (x) (+ (first masked-jumps) x)) '(0 1 1))
         (map (lambda (x) (+ (second masked-jumps) x)) '(1 0 1))
         (map (lambda (x) (+ (third masked-jumps) x)) '(1 1 0))))
  (if (zero? obstacle) free-jumps (list-set free-jumps (sub1 obstacle) 1919810)))

(define/contract (min-side-jumps obstacles)
  (-> (listof exact-integer?) exact-integer?)
  (apply min (foldl jumps '(1 0 1) obstacles)))

(min-side-jumps '(0 0 3 1 0 1 0 2 3 1 0))
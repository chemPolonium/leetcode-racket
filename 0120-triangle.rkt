#lang racket

(define/contract (minimum-total triangle)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define (t l) (if (null? (cdr l)) l (cons (min (car l) (cadr l)) (t (cdr l)))))
  (define (it c p) (map + (cons (car p) (t p)) c))
  (apply min (foldl it (car triangle) (cdr triangle))))

(minimum-total '((2) (3 4) (6 5 7) (4 1 8 3)))
(minimum-total '((-10)))

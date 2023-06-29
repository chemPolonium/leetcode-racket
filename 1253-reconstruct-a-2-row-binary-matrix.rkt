#lang racket

(define/contract (reconstruct-matrix upper lower colsum)
  (-> exact-integer? exact-integer? (listof exact-integer?) (listof (listof exact-integer?)))
  (let loop ([u upper] [l lower] [sl colsum] [ul '()] [ll '()])
    (cond [(null? sl) (if (or (positive-integer? u) (positive-integer? l))
                          null
                          (list (reverse ul) (reverse ll)))]
          [(= 0 (car sl)) (loop u l (cdr sl) (cons 0 ul) (cons 0 ll))]
          [(= 2 (car sl))
           (if (and (positive-integer? u) (positive-integer? l))
               (loop (sub1 u) (sub1 l) (cdr sl) (cons 1 ul) (cons 1 ll))
               null)]
          [(and (positive-integer? u) (>= u l))
           (loop (sub1 u) l (cdr sl) (cons 1 ul) (cons 0 ll))]
          [(and (positive-integer? l) (nonnegative-integer? u))
           (loop u (sub1 l) (cdr sl) (cons 0 ul) (cons 1 ll))]
          [else null])))

(reconstruct-matrix 4 7 '(2 1 2 2 1 1 1))
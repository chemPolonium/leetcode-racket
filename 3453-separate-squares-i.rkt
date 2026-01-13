#lang racket

(define/contract (separate-squares squares)
  (-> (listof (listof exact-integer?)) flonum?)
  (define ds (map second squares))
  (define ls (map third squares))
  (define us (map + ds ls))
  (define hsa (/ (apply + (map sqr ls)) 2.0))
  (define (f yq)
    (foldl (lambda (d l u s)
             (+ s (* l (- (min u (max d yq)) d))))
           0 ds ls us))
  (let iter ([d (apply min ds)]
             [u (apply max us)])
    (if (< (- u d) 1e-5)
        d
        (let* ([m (/ (+ u d) 2.0)]
               [mv (f m)])
          (if (< mv hsa)
              (iter m u)
              (iter d m))))))

(separate-squares '((0 0 1) (2 2 1)))

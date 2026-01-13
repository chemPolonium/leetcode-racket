#lang racket

(define/contract (min-time-to-visit-all-points points)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (let iter ([p (car points)] [ps (cdr points)] [s 0])
    (if (null? ps)
        s
        (iter (car ps) (cdr ps) (+ s (max (abs (- (car p) (caar ps)))
                                          (abs (- (cadr p) (cadar ps)))))))))

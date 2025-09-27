#lang racket

(define/contract (largest-triangle-area points)
  (-> (listof (listof exact-integer?)) flonum?)
  (define (cross-product p1 p2 p3)
    (define x1 (- (first p2) (first p1)))
    (define y1 (- (second p2) (second p1)))
    (define x2 (- (first p3) (first p1)))
    (define y2 (- (second p3) (second p1)))
    (- (* x1 y2) (* y1 x2)))
  (define v (list->vector points))
  (define n (vector-length v))
  (exact->inexact
   (for*/fold ([a 0])
              ([i (in-range n)]
               [j (in-range i n)]
               [k (in-range j n)])
     (max a (/ (abs (cross-product (vector-ref v i)
                                   (vector-ref v j)
                                   (vector-ref v k)))
               2)))))
